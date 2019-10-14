{
	GNU License
	
	2019/10/14 : first make. super poor code :)
}

unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, XPMan, MMSystem, pngimage, jpeg, TntClasses,
  TntSysUtils,

  {$I headers.inc}

  ShellApi, ShlObj, ImgList, ComCtrls, VirtualTrees, JvToolEdit, JvExStdCtrls,
  JvButton, JvCtrls, JclFileUtils, JvExMask, JvExControls, JvXPCore,
  JvXPButtons, JvArrowButton, JkhFlatButton, Menus{, JwaShLWAPI};

type
  TTagedBitmap = class(TBitmap)
  private
  public
    Tag: Integer;
    procedure Assign(Source: TPersistent); override;
  end;

  TUpdateThread = class(TThread)
  private
    FTargetList: TList;
    FTargetFileName: String;
    FItemHeight: Integer;

    procedure Execute; override;
  public
    ImageBuffer: TTagedBitmap;
    TargetEtcInfo: String;

    constructor Create(const ATargetList: TList; ATargetFileName: String; AItemHeight: Integer);
  end;

  TAutoPlayThread = class(TThread)
  private
    fmt_ctx: PAVFormatContext;
    video_dec_ctx: PAVCodecContext;
    video_stream: PAVStream;
    // video_codec: PAVCodec;
    src_filename: PAnsiChar;

    video_stream_idx: Integer;
    frame: PAVFrame;
    video_frame_count: Integer;

    pkt: TAVPacket;
    ret: Integer;
    FItemHeight: Integer;

    FTargetFileName: String;

    procedure DoWork;
    procedure Execute; override;
  public
    TimeTerm: Integer;
    ProcessedFile,
    TargetFile: String;
    CurrentImage: TTagedBitmap;
    SeekRequestTime: Int64;

    constructor Create;
    destructor Destroy;

    procedure PrepareFile(ATargetFile: String);
    procedure SeekRequest(ASeekTime: Int64);
  end;

  TMainform = class(TForm)
    VirtualDrawTree1: TVirtualDrawTree;
    XPManifest1: TXPManifest;
    Timer1: TTimer;
    ThumbUpdate: TTimer;
    SystemImages: TImageList;
    Panel1: TPanel;
    BrowerEditor: TJvDirectoryEdit;
    ImageList1: TImageList;
    FunctionButton: TJkhFlatButton;
    MainPopupMenu: TPopupMenu;
    Info1: TMenuItem;
    N1: TMenuItem;
    Config1: TMenuItem;
    Menu_ConfRememberLastUsedPath: TMenuItem;
    N2: TMenuItem;
    File1: TMenuItem;
    Savethumbnails1: TMenuItem;
    procedure BrowerEditorChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure VirtualDrawTree1DrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure FormShow(Sender: TObject);
    procedure ThumbUpdateTimer(Sender: TObject);
    procedure VirtualDrawTree1FreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure VirtualDrawTree1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure VirtualDrawTree1FocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure VirtualDrawTree1DblClick(Sender: TObject);
    procedure VirtualDrawTree1NodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure VirtualDrawTree1CompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FunctionButtonClick(Sender: TObject);
    procedure Info1Click(Sender: TObject);
  private
    FScreenLogPixels: Integer;
    FAutoUpdateThread: TAutoPlayThread;
    FCurrentPath: String;
    FUpdateThreadCnt: Integer;
    FUpdateThreadMax: Integer;

    FConfigFile: TStringList;

    procedure WMUSER(var msg: TMessage); message WM_USER;
  public
    procedure MakeVDTList(Target: WideString);
  end;

const
  Const_VersionString: String = 'Version 0.1';

  Const_ConfigFileName: String = 'config.ini';
  Const_VarRememberLastPath: String = 'RememberLastPath';
  Const_VarLastPath: String = 'LastPath';

  BoolToStr: Array[Boolean] of String = ('false', 'true');

var
  Mainform: TMainform;
  ThreadSyncTime: Longword;
  ConfigFilePath: String;

implementation

type
  // This data record contains all necessary information about a particular file system object.
  // This can either be a folder (virtual or real) or an image file.
  PShellObjectData = ^TShellObjectData;
  TShellObjectData = record
    FullPath,
    Display,
    EtcInfo: UnicodeString;
    WorkFlag: Integer;
    Attributes: Cardinal;
    OpenIndex,
    CloseIndex: Integer;      // image indices into the system image list
    Image: TTagedBitmap;
    Images: TList;
    Properties: UnicodeString;   // some image properties, preformatted
  end;

{$R *.dfm}

function GetIconIndex(const Name: WideString; Flags: Cardinal): Integer;
var
  SFI: TSHFileInfoW;
begin
  if SHGetFileInfoW(PChar(Name), 0, SFI, SizeOf(TSHFileInfoW), Flags) = 0 then
     Result := -1
  else
     Result := SFI.iIcon;
end;

procedure GetOpenAndClosedIcons(const Name: WideString; var Open, Closed: Integer);
begin
  Closed := GetIconIndex(Name, SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  Open := GetIconIndex(Name, SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON);
end;

function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall; external 'shlwapi.dll' name 'PathCanonicalizeW';

function RelToAbs(const RelPath: WideString): WideString;
var
  Dst: array[0..MAX_PATH-1] of WideChar;
begin
  PathCanonicalize(@Dst[0], @(RelPath[1]));
  result := Dst;
end;

{ TTagedBitmap }

procedure TTagedBitmap.Assign(Source: TPersistent);
begin
  inherited;

  If Source is TTagedBitmap Then
  Begin
     Self.Tag := TTagedBitmap(Source).Tag;
  End;
end;

{ TUpdateThread }

constructor TUpdateThread.Create(const ATargetList: TList;
  ATargetFileName: String; AItemHeight: Integer);
begin
  SendMessage(Mainform.Handle, WM_USER, 0, 1);

  FTargetList := ATargetList;
  FTargetFileName := ATargetFileName;
  FItemHeight := AItemHeight;

  Inherited Create(False);

  Self.FreeOnTerminate := True;
end;

procedure TUpdateThread.Execute;
var
  fmt_ctx: PAVFormatContext;
  video_dec_ctx: PAVCodecContext;
  video_stream: PAVStream;
  // video_codec: PAVCodec;
  src_filename: PAnsiChar;

  video_stream_idx: Integer;
  frame: PAVFrame;
  video_frame_count: Integer;

  pkt: TAVPacket;
  ret: Integer;

  fullLength: Int64;
  seek_target,
  SeekTime, SeekCandidate: Int64;
  TimeDivVar: Int64;
  RequestThumb: Integer;

  LoopVar: Integer;
  TempBitmap: TTagedBitmap;

  KeepSyncTime: Longword;

  function PPtrIdx(P: PPAVStream; I: Integer): PAVStream;
  begin
    Inc(P, I);
    Result := P^;
  end;

  function open_codec_context(fmt_ctx: PAVFormatContext; type_: TAVMediaType): Integer;
  var
    ret: Integer;
    st: PAVStream;
    dec_ctx: PAVCodecContext;
    avdec: PAVCodec;
    opts: PAVDictionary;
    stream_idx: Integer;
  begin
    avdec := nil;
    opts := nil;

    ret := av_find_best_stream(fmt_ctx, type_, -1, -1, @avdec, 0);
    if ret < 0 then
    begin
      // Writeln(ErrOutput, Format('Could not find %s stream in input file "%s"',
      //            [av_get_media_type_string(type_), src_filename]));
      Result := ret;
      Exit;
    end
    else
    begin
      stream_idx := ret;
      st := PPtrIdx(fmt_ctx.streams, stream_idx);

      dec_ctx := avcodec_alloc_context3(avdec);
      if not Assigned(dec_ctx) then
      begin
        // Writeln(ErrOutput, 'Failed to allocate codec');
        Result := AVERROR_EINVAL;
        Exit;
      end;

      ret := avcodec_parameters_to_context(dec_ctx, st.codecpar);
      if ret < 0 then
      begin
         // Writeln(ErrOutput, 'Failed to copy codec parameters to codec context');
         Result := ret;
         Exit;
      end;

      (* Init the video decoder *)
      // av_dict_set(@opts, 'preset', 'veryfast', 0 ); // 'flags2', '+export_mvs', 0);
      ret := avcodec_open2(dec_ctx, avdec, @opts);
      if ret < 0 then
      begin
        Writeln(ErrOutput, Format('Failed to open %s codec',
                [av_get_media_type_string(type_)]));
        Result := ret;
        Exit;
      end;

      video_stream_idx := stream_idx;
      video_stream := PPtrIdx(fmt_ctx.streams, video_stream_idx);
      video_dec_ctx := dec_ctx;
    end;

    Result := 0;
  end;

  function decode_packet(const pkt: PAVPacket): Integer;
  var
    ret: Integer;
    i: Integer;
    sd: PAVFrameSideData;
    mvs: PAVMotionVector;
    mv: PAVMotionVector;
    sws_ctx: PSwsContext;

    dst_data: array[0..3] of PByte;
    dst_linesize: array[0..3] of Integer;
    dst_height, dst_width: Integer;
  begin
    ret := avcodec_send_packet(video_dec_ctx, pkt);
    if ret < 0 then
    begin
      // Writeln(ErrOutput, Format('Error while sending a packet to the decoder (%s)', [av_err2str(ret)]));
      Result := ret;
      Exit;
    end;

    while ret >= 0 do
    begin
      ret := avcodec_receive_frame(video_dec_ctx, frame);
      if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
        Break
      else if ret < 0 then
      begin
        // Writeln(ErrOutput, Format('Error while receiving a frame from the decoder: %s', [av_err2str(ret)]));
        Result := ret;
        Exit;
      end;

      if ret >= 0 then
      begin
        If video_dec_ctx.height <= 0 Then Exit;
        dst_width := Round( video_dec_ctx.width * (FItemHeight / video_dec_ctx.height) );
        dst_height := FItemHeight;

        if (dst_width <> 0) and (dst_height <> 0) then
        Begin
          ret := av_image_alloc(@dst_data[0], @dst_linesize[0], dst_width, dst_height, AV_PIX_FMT_RGB32, 1);

          sws_ctx := sws_getContext ( video_dec_ctx.width, video_dec_ctx.height, video_dec_ctx.pix_fmt,
                                      dst_width, dst_height, AV_PIX_FMT_RGB32, SWS_BICUBIC, nil, nil, nil );
          sws_scale ( sws_ctx, @frame.data, @frame.linesize,
                      0, frame.height, @dst_data, @dst_linesize );

          Try
             ImageBuffer.PixelFormat := pf32bit;
             ImageBuffer.Width := dst_width;
             ImageBuffer.Height := dst_height;

             for i := 0 to ImageBuffer.Height - 1 do
               CopyMemory ( ImageBuffer.ScanLine[i],
                   pointer (integer (dst_data[0]) + ImageBuffer.Width * 4 * i), ImageBuffer.Width * 4 );
          Except
          End;

          av_freep(@dst_data[0]);
          sws_freeContext ( sws_ctx );

          Result := 1;
        end;

        Exit;
      end;
    end;

    Result := 0;
  end;

  function BuildFrame: Boolean;
  var
    ReadNext: Integer;
  begin
    Result := False;
    ReadNext := 0;
    // ImageBuffer.Width := 0;
    ImageBuffer.Free;
    ImageBuffer := TTagedBitmap.Create;
    
    while av_read_frame(fmt_ctx, @pkt) >= 0 do
    begin
       if pkt.stream_index = video_stream_idx then
       Begin
          ret := decode_packet(@pkt);
          av_packet_unref(@pkt);

          if ret < 0 then Break;

          // Find frame.
          if ret > 0 then
          Begin
             Result := True;
             Exit;
          End;
       End
       Else
       Begin
          av_packet_unref(@pkt);
       End;
    end;

    // not care cache.
    // decode_packet(@pkt);
  end;

begin
  // TargeTTagedBitmap.LoadFromFile('C:\Project\Component\VirtualTreeview V5.3.0\Demos\Advanced\Res\Cyrillic.bmp');
  KeepSyncTime := ThreadSyncTime;
  TargetEtcInfo := '';

  fmt_ctx := Nil;
  video_dec_ctx := Nil;
  video_stream := Nil;
  // video_codec := Nil;
  src_filename := Nil;

  video_stream_idx := -1;
  frame := nil;
  video_frame_count := 0;

  FillChar(pkt, Sizeof(TAVPacket), 0);

  src_filename := PAnsiChar(FTargetFileName);

  Try
     if avformat_open_input(@fmt_ctx, src_filename, nil, nil) < 0 then
     // if av_open_input_file(@fmt_ctx, src_filename, nil, 0, nil) < 0 Then // old
     begin
        // Writeln(ErrOutput, Format('Could not open source file %s', [src_filename]));
        ret := 1;
        Exit;
     end;

     if avformat_find_stream_info(fmt_ctx, nil) < 0 then
     begin
        // Writeln(ErrOutput, 'Could not find stream information');
        ret := 1;
        Exit;
     end;

     Try
        av_dump_format(fmt_ctx, 0, src_filename, 0);
        ret := 0;
     Except
        ret := 1;
        Exit;
     End;
  Finally
     If ret <> 0 then
     Begin
        SendMessage(Mainform.Handle, WM_USER, 0, 2);
        avformat_close_input(@fmt_ctx);
     End;
  End;

  ImageBuffer := TTagedBitmap.Create;  
  Try
     open_codec_context(fmt_ctx, AVMEDIA_TYPE_VIDEO);

     if not Assigned(video_stream) then
     begin
        // Writeln(ErrOutput, 'Could not find video stream in the input, aborting');
        ret := 1;
        Exit;
     end;

     frame := av_frame_alloc();
     if not Assigned(frame) then
     begin
        // Writeln(ErrOutput, 'Could not allocate frame');
        ret := AVERROR_ENOMEM;
        Exit;
     end;

     //
     fullLength := fmt_ctx.duration;
     // Caption := IntToStr(fullLength);

     If fullLength > 0 Then
     Begin
        RequestThumb := 100;
        TimeDivVar := 180000000;
        If TimeDivVar > fullLength Then
           TimeDivVar := fullLength;
        SeekTime := 0;

        // Must read first stream.
        BuildFrame;

        If (fullLength > 1000) then
        Begin
           SeekCandidate := TimeDivVar - Random(TimeDivVar div 3);
           If SeekCandidate > 0 Then
           Begin
              seek_target := av_rescale_q(SeekCandidate, AV_TIME_BASE_Q, video_stream.time_base);
              avformat_seek_file(fmt_ctx, video_stream_idx, seek_target, seek_target, seek_target, AVSEEK_FLAG_BACKWARD or AVSEEK_FLAG_FRAME);
              avcodec_flush_buffers(video_dec_ctx);
              // av_seek_frame(fmt_ctx, video_stream_idx, seek_target, AVSEEK_FLAG_ANY);
           End;
        End;

        For LoopVar := 1 to RequestThumb do
        Begin
           If Terminated Then Break;
           
           If LoopVar > 1 Then
           Begin
              // av_seek_frame(fmt_ctx, video_stream_idx, SeekTime, AVSEEK_FLAG_BYTE); // AVSEEK_FLAG_ANY);
              SeekCandidate := SeekTime + Random(TimeDivVar div 3);
              If SeekCandidate > fullLength Then
                 SeekCandidate := fullLength;
              seek_target := av_rescale_q(SeekTime, AV_TIME_BASE_Q, video_stream.time_base);
              avformat_seek_file(fmt_ctx, video_stream_idx, seek_target, seek_target, seek_target, AVSEEK_FLAG_BACKWARD or AVSEEK_FLAG_FRAME);
              avcodec_flush_buffers(video_dec_ctx);
           End;

           If BuildFrame and (Not ImageBuffer.Empty) Then
           Begin
              TempBitmap := TTagedBitmap.Create;
              TempBitmap.Assign(ImageBuffer);
              TempBitmap.Tag := video_stream.cur_dts;

              // 동기화가 되어 있을때만 처리한다. 아니면 바로 종료시킬것임
              // 다른 쓰레드는 역으로 SendMessage 에서 가져가지만 이쪽은 동시성의 처리를 수행하므로 이쪽에서 푸쉬한다.
              // 또 Free 도 저쪽에서 하므로 이쪽에서 Free 하지 않음.
              If KeepSyncTime = ThreadSyncTime Then
                 FTargetList.Add(TempBitmap)
              Else
              Begin
                 TempBitmap.Free;
                 Break;
              End;
           End;

           SeekTime := SeekTime + TimeDivVar;

           If ImageBuffer.Empty Then Break;
        End;

        // TargetEtcInfo := av_ts2timestr(fullLength, @(video_stream.time_base));
     End;

     SendMessage(Mainform.Handle, WM_USER, 0, 1000);
     // TargeTTagedBitmap.Assign(ImageBuffer);
     // ImageBuffer
  Finally
     SendMessage(Mainform.Handle, WM_USER, 0, 2);

     avcodec_free_context(@video_dec_ctx);
     avformat_close_input(@fmt_ctx);
     av_frame_free(@frame);
     // Result := Ord(ret < 0);
  End;
end;

{ TAutoPlayThread }

constructor TAutoPlayThread.Create;
begin
  inherited Create(False);

  FItemHeight := 100;

  SeekRequestTime := -1;  
  ProcessedFile := '';
  CurrentImage := TTagedBitmap.Create;
  FreeOnTerminate := True;
end;

destructor TAutoPlayThread.Destroy;
begin
//     CurrentImage: TTagedBitmap;
  inherited;

  CurrentImage.Free;
end;

procedure TAutoPlayThread.DoWork;
var
  fmt_ctx: PAVFormatContext;
  video_dec_ctx: PAVCodecContext;
  video_stream: PAVStream;
  // video_codec: PAVCodec;
  src_filename: PAnsiChar;

  video_stream_idx: Integer;
  frame: PAVFrame;
  video_frame_count: Integer;

  pkt: TAVPacket;
  ret: Integer;

  fullLength: Int64;
  seek_target,
  SeekTime: Int64;
  TimeDivVar: Int64;
  RequestThumb: Integer;

  LoopVar: Integer;
  ImageBuffer: TTagedBitmap;
  ConvertFlag: Boolean;
  StartTime: Longword;
  EstiTime: Longword;

  EnterFileName: String;

  function PPtrIdx(P: PPAVStream; I: Integer): PAVStream;
  begin
    Inc(P, I);
    Result := P^;
  end;

  function open_codec_context(fmt_ctx: PAVFormatContext; type_: TAVMediaType): Integer;
  var
    ret: Integer;
    st: PAVStream;
    dec_ctx: PAVCodecContext;
    avdec: PAVCodec;
    opts: PAVDictionary;
    stream_idx: Integer;
  begin
    avdec := nil;
    opts := nil;

    ret := av_find_best_stream(fmt_ctx, type_, -1, -1, @avdec, 0);
    if ret < 0 then
    begin
      // Writeln(ErrOutput, Format('Could not find %s stream in input file "%s"',
      //            [av_get_media_type_string(type_), src_filename]));
      Result := ret;
      Exit;
    end
    else
    begin
      stream_idx := ret;
      st := PPtrIdx(fmt_ctx.streams, stream_idx);

      dec_ctx := avcodec_alloc_context3(avdec);
      if not Assigned(dec_ctx) then
      begin
        // Writeln(ErrOutput, 'Failed to allocate codec');
        Result := AVERROR_EINVAL;
        Exit;
      end;

      ret := avcodec_parameters_to_context(dec_ctx, st.codecpar);
      if ret < 0 then
      begin
         // Writeln(ErrOutput, 'Failed to copy codec parameters to codec context');
         Result := ret;
         Exit;
      end;

      (* Init the video decoder *)
      // av_dict_set(@opts, 'preset', 'veryfast', 0 ); // 'flags2', '+export_mvs', 0);
      ret := avcodec_open2(dec_ctx, avdec, @opts);
      if ret < 0 then
      begin
        Writeln(ErrOutput, Format('Failed to open %s codec',
                [av_get_media_type_string(type_)]));
        Result := ret;
        Exit;
      end;

      video_stream_idx := stream_idx;
      video_stream := PPtrIdx(fmt_ctx.streams, video_stream_idx);
      video_dec_ctx := dec_ctx;
    end;

    Result := 0;
  end;

  function decode_packet(const pkt: PAVPacket): Integer;
  var
    ret: Integer;
    i: Integer;
    sd: PAVFrameSideData;
    mvs: PAVMotionVector;
    mv: PAVMotionVector;
    sws_ctx: PSwsContext;

    dst_data: array[0..3] of PByte;
    dst_linesize: array[0..3] of Integer;
    dst_height, dst_width: Integer;
  begin
    ret := avcodec_send_packet(video_dec_ctx, pkt);
    if ret < 0 then
    begin
      // Writeln(ErrOutput, Format('Error while sending a packet to the decoder (%s)', [av_err2str(ret)]));
      Result := ret;
      Exit;
    end;

    while ret >= 0 do
    begin
      If Terminated Then Break;

      ret := avcodec_receive_frame(video_dec_ctx, frame);
      if (ret = AVERROR_EAGAIN) or (ret = AVERROR_EOF) then
        Break
      else if ret < 0 then
      begin
        // Writeln(ErrOutput, Format('Error while receiving a frame from the decoder: %s', [av_err2str(ret)]));
        Result := ret;
        Exit;
      end;

      if (ret >= 0) and ConvertFlag then
      begin
        ConvertFlag := False;
        If video_dec_ctx.height <= 0 Then Exit;
        dst_width := Round( video_dec_ctx.width * (FItemHeight / video_dec_ctx.height) );
        dst_height := FItemHeight;

        if (dst_width <> 0) and (dst_height <> 0) then
        Begin
          ret := av_image_alloc(@dst_data[0], @dst_linesize[0], dst_width, dst_height, AV_PIX_FMT_RGB32, 1);

          sws_ctx := sws_getContext ( video_dec_ctx.width, video_dec_ctx.height, video_dec_ctx.pix_fmt,
                                      dst_width, dst_height, AV_PIX_FMT_RGB32, SWS_BICUBIC, nil, nil, nil );
          sws_scale ( sws_ctx, @frame.data, @frame.linesize,
                      0, frame.height, @dst_data, @dst_linesize );

          Try
             ImageBuffer.PixelFormat := pf32bit;
             ImageBuffer.Width := dst_width;
             ImageBuffer.Height := dst_height;

             for i := 0 to ImageBuffer.Height - 1 do
               CopyMemory ( ImageBuffer.ScanLine[i],
                   pointer (integer (dst_data[0]) + ImageBuffer.Width * 4 * i), ImageBuffer.Width * 4 );
          Except
          End;

          av_freep(@dst_data[0]);
          sws_freeContext ( sws_ctx );

          Result := 1;
        end;

        Exit;
      end;
    end;

    Result := 0;
  end;

  function BuildFrame: Boolean;
  begin
    Result := False;
    // ImageBuffer.Width := 0;
    while av_read_frame(fmt_ctx, @pkt) >= 0 do
    begin
       If Terminated Then Break;

       if pkt.stream_index = video_stream_idx then
       Begin
          ret := decode_packet(@pkt);
          av_packet_unref(@pkt);

          if ret < 0 then Break;

          // Find frame.
          if ret >= 0 then
          Begin
             Result := True;
             Exit;
          End;
       End
       Else
       Begin
          av_packet_unref(@pkt);
       End;
    end;

    // not care cache.
    // decode_packet(@pkt);
  end;

begin
  // TargeTTagedBitmap.LoadFromFile('C:\Project\Component\VirtualTreeview V5.3.0\Demos\Advanced\Res\Cyrillic.bmp');

  fmt_ctx := Nil;
  video_dec_ctx := Nil;
  video_stream := Nil;
  // video_codec := Nil;
  src_filename := Nil;

  video_stream_idx := -1;
  frame := nil;
  video_frame_count := 0;

  FillChar(pkt, Sizeof(TAVPacket), 0);

  src_filename := PAnsiChar(FTargetFileName);

  Try
     if avformat_open_input(@fmt_ctx, src_filename, nil, nil) < 0 then
     // if av_open_input_file(@fmt_ctx, src_filename, nil, 0, nil) < 0 Then // old
     begin
        // Writeln(ErrOutput, Format('Could not open source file %s', [src_filename]));
        ret := 1;
        Exit;
     end;

     if avformat_find_stream_info(fmt_ctx, nil) < 0 then
     begin
        // Writeln(ErrOutput, 'Could not find stream information');
        ret := 1;
        Exit;
     end;

     Try
        av_dump_format(fmt_ctx, 0, src_filename, 0);
        ret := 0;
     Except
        ret := 1;
        Exit;
     End;
  Finally
     If ret <> 0 then
        avformat_close_input(@fmt_ctx);
  End;

  ImageBuffer := TTagedBitmap.Create;
  Try
     open_codec_context(fmt_ctx, AVMEDIA_TYPE_VIDEO);

     if not Assigned(video_stream) then
     begin
        // Writeln(ErrOutput, 'Could not find video stream in the input, aborting');
        ret := 1;
        Exit;
     end;

     frame := av_frame_alloc();
     if not Assigned(frame) then
     begin
        // Writeln(ErrOutput, 'Could not allocate frame');
        ret := AVERROR_ENOMEM;
        Exit;
     end;

     //
     fullLength := fmt_ctx.duration div 1000;
     // Caption := IntToStr(fullLength);

     If fullLength > 0 Then
     Begin
        EstiTime := 0;
        While FTargetFileName = TargetFile do
        Begin
           If Self.Terminated Then Break;

           If SeekRequestTime >= 0 Then
           Begin
              seek_target := SeekRequestTime;
              // seek_target := av_rescale_q(SeekRequestTime, AV_TIME_BASE_Q, video_stream.time_base);
              avformat_seek_file(fmt_ctx, video_stream_idx, seek_target, seek_target, seek_target, AVSEEK_FLAG_BACKWARD or AVSEEK_FLAG_FRAME);
              avcodec_flush_buffers(video_dec_ctx);
              SeekRequestTime := -1;
           End;

           StartTime := TimeGetTime;
           BuildFrame;
           EstiTime := EstiTime + (TimeGetTime - StartTime);

           If EstiTime > 500 Then
           Begin
              ConvertFlag := True; // 다음
           End;

           If Not ImageBuffer.Empty Then
           Begin
              CurrentImage.Assign(ImageBuffer);
              CurrentImage.Tag := video_stream.cur_dts;
              ImageBuffer.Free;
              ImageBuffer := TTagedBitmap.Create;

              SendMessage(Mainform.Handle, WM_USER, 0, 2000);
              EstiTime := 0;
           End;
        End;
     End;
  Finally
     avcodec_free_context(@video_dec_ctx);
     avformat_close_input(@fmt_ctx);
     av_frame_free(@frame);

     // Result := Ord(ret < 0);
     ImageBuffer.Free;
  End;
end;

procedure TAutoPlayThread.Execute;
begin
  While Not Terminated do
  Begin
     If (Not FileExists(TargetFile)) or (ProcessedFile = TargetFile) Then
     Begin
        If TargetFile = '' Then
           ProcessedFile := '';
        Sleep(100);
        Continue;
     End;

     FTargetFileName := TargetFile;
     Try
        DoWork;
     Except
     End;
     ProcessedFile := FTargetFileName;
  End;
end;

procedure TAutoPlayThread.PrepareFile(ATargetFile: String);
begin
  TargetFile := ATargetFile;
end;

procedure TAutoPlayThread.SeekRequest(ASeekTime: Int64);
begin
  SeekRequestTime := ASeekTime;
end;

{ TMainform }

procedure TMainform.BrowerEditorChange(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TMainform.MakeVDTList(Target: WideString);
var
  // Data,
  ChildData: PShellObjectData;
  SR: TSearchRecW;
  ChildNode: PVirtualNode;
  NewName: WideString;
  ParsedExt: String;
begin
  If Not DirectoryExists(Target) Then Exit;
  If Target = FCurrentPath Then Exit;

  FCurrentPath := Target;

  ThreadSyncTime := TimeGetTime();
  FAutoUpdateThread.PrepareFile('');

  VirtualDrawTree1.NodeDataSize := SizeOf(TShellObjectData);
  VirtualDrawTree1.RootNodeCount := 0;

  VirtualDrawTree1.BeginUpdate;
  if WideFindFirst(WideIncludeTrailingBackslash(Target) + '*.*', faAnyFile, SR) = 0 then
  begin
     Screen.Cursor := crHourGlass;
     Try
        repeat
           if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Attr and faDirectory = 0) then
           begin
              ParsedExt := WideExtractFileExt(SR.Name);
              If ParsedExt = '' then Continue;
              If Pos(LowerCase(ParsedExt)+';', '.mkv;.mp4;.m4a;.m4v;.f4v;.f4a;.m4b;.m4r;.f4b;.mov;.3gp;.3gp2;.3g2;.3gpp;.3gpp2;.ogg;.oga;.ogv;.ogx;.wmv;.wma;.asf;.webm;.flv;.avi;') <= 0 Then continue;
              ChildNode := VirtualDrawTree1.AddChild( VirtualDrawTree1.RootNode );
              ChildData := VirtualDrawTree1.GetNodeData(ChildNode);

              If ChildData = Nil Then Continue;

              ChildData.Display := SR.Name;
              ChildData.EtcInfo := '';
              NewName := WideIncludeTrailingBackslash(Target) + SR.Name;
              ChildData.FullPath := NewName;
              ChildData.WorkFlag := -1;
              ChildData.Image := TTagedBitmap.Create;
              ChildData.Images := TList.Create;
              GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex, ChildData.CloseIndex);

              ChildData.Attributes := 0;

              {
              if (SR.Attr and faDirectory <> 0) or CanDisplay(NewName) then
              begin
                 ChildNode := Sender.AddChild(Node);
                 ChildData := Sender.GetNodeData(ChildNode);
                 ChildData.FullPath := NewName;
                 ChildData.Attributes := ReadAttributes(NewName);
                 if (ChildData.Attributes and SFGAO_FOLDER) = 0 then
                   ChildData.Properties := Format('%n KB, ', [SR.Size / 1024]);
                 GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex, ChildData.CloseIndex);

                 Sender.ValidateNode(Node, False);
              end;
              }
           end
           Else If (SR.Attr and faDirectory <> 0) and (not (SR.Name = '.')) then
           Begin
              ChildNode := VirtualDrawTree1.AddChild( VirtualDrawTree1.RootNode );
              ChildNode.NodeHeight := Canvas.TextHeight('A') + 2;
              ChildData := VirtualDrawTree1.GetNodeData(ChildNode);
              If ChildData = Nil Then Continue;

              ChildData.Display := SR.Name;
              ChildData.EtcInfo := '';
              NewName := WideIncludeTrailingBackslash(Target) + SR.Name;
              ChildData.FullPath := NewName;
              ChildData.WorkFlag := 0;
              ChildData.Image := TTagedBitmap.Create;
              ChildData.Images := TList.Create;
              GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex, ChildData.CloseIndex);

              ChildData.Attributes := 1;
           End;
        until WideFindNext(SR) <> 0;
     Finally
        VirtualDrawTree1.SortTree(VirtualDrawTree1.Header.SortColumn, VirtualDrawTree1.Header.SortDirection);
        VirtualDrawTree1.EndUpdate;
        WideFindClose(SR);
        Screen.Cursor := crDefault;
     End;
  end;
end;

procedure TMainform.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  MakeVDTList(BrowerEditor.Text);
end;

procedure TMainform.VirtualDrawTree1DrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var
  Data: PShellObjectData;
  WorkBitmap: TTagedBitmap;
  LoopVar, LoopMax, StartIdx,
  X: Integer;
  S: UnicodeString;
  R: TRect;
  lf: LOGFONT;
  PositionTime, PositionIndex,
  LastIndex: Integer;
begin
  with Sender as TVirtualDrawTree, PaintInfo do
  begin
     Data := Sender.GetNodeData(Node);

     FillChar(lf, SizeOf(lf), Byte(0));
     lf.lfHeight := -MulDiv(8, FScreenLogPixels, 72);
     lf.lfWidth := 0;
     lf.lfWeight := FW_NORMAL;
     lf.lfQuality := ANTIALIASED_QUALITY;
     lf.lfCharSet := HANGEUL_CHARSET;
     StrCopy(lf.lfFaceName, PChar(Font.Name));
     Canvas.Font.Handle := CreateFontIndirect(lf);

     if (Column = FocusedColumn) and (Selected[Node]) then
        Canvas.Font.Color := clHighlightText
     else
        if (Data.Attributes and SFGAO_COMPRESSED) <> 0 then
           Canvas.Font.Color := clBlue
        else
           Canvas.Font.Color := clWindowText;

     SetBKMode(Canvas.Handle, TRANSPARENT);

     R := ContentRect;
     InflateRect(R, -TextMargin, 0);
     Dec(R.Right);
     Dec(R.Bottom);
     S := '';
     case Column of
        0:
        begin
           S := Data.Display;
           if Length(S) > 0 then
           begin
              with R do
              begin
                if (NodeWidth - 2 * Margin) > (Right - Left) then
                   S := ShortenString(Canvas.Handle, S, Right - Left);
              end;
              DrawTextW(Canvas.Handle, PWideChar(S), Length(S), R, DT_TOP or DT_LEFT or DT_VCENTER or DT_SINGLELINE); //  or DT_WORDBREAK or DT_EDITCONTROL
           end;
        end;
        1:
        begin
           X := ContentRect.Left;
           StartIdx := 0;
           PositionTime := -1;
           PositionIndex := -1;

           // 처리해야할 파일만 이 값이 0 이다.
           If Data.Attributes = 0 Then
           Begin
              if Assigned(Data.Image) and (Not Data.Image.Empty) then
              Begin
                 // BitBlt(Canvas.Handle, X, ContentRect.Top, Data.Image.Width, Data.Image.Height, Data.Image.Canvas.Handle, 0, 0, SRCCOPY);
                 // X := X + Data.Image.Width + 2;
                 // StartIdx := 1;
                 PositionTime := Data.Image.Tag;
                 PositionIndex := StartIdx;
              end;

              If Assigned(Data.Images) and (Data.Images.Count > 0) Then
              Begin
                 LoopMax := Data.Images.Count-1;
                 //If LoopMax >= 3 Then LoopMax := 2;
                 For LoopVar := StartIdx to LoopMax do
                 Begin
                    WorkBitmap := TTagedBitmap(Data.Images.Items[LoopVar]);
                    If (PositionTime > 0) and (PositionTime >= WorkBitmap.Tag) Then
                       PositionIndex := LoopVar;

                    // 영역 안의것만 처리하자.
                    If ((X + WorkBitmap.Width) > 0) and (X < R.Right) Then
                       BitBlt(Canvas.Handle, X, ContentRect.Top, WorkBitmap.Width, WorkBitmap.Height, WorkBitmap.Canvas.Handle, 0, 0, SRCCOPY);

                    X := X + WorkBitmap.Width + 2;
                 End;
              End;

              If PositionIndex >= 0 Then
              Begin
                 BitBlt(Canvas.Handle, ContentRect.Left+(PositionIndex*(Data.Image.Width+2)), ContentRect.Top, Data.Image.Width, Data.Image.Height, Data.Image.Canvas.Handle, 0, 0, SRCCOPY);
              End;
           End;
        end;
     end;
  end;
end;

procedure TMainform.FormShow(Sender: TObject);
begin
  MakeVDTList(BrowerEditor.Text);
end;

procedure TMainform.ThumbUpdateTimer(Sender: TObject);
var
  LoopVar: Integer;
  WorkNode: PVirtualNode;
  ChildData: PShellObjectData;
  Updated: Boolean;
begin
  Updated := False;
  WorkNode := VirtualDrawTree1.GetFirst();
  If WorkNode = Nil Then Exit;

  While True do
  Begin
     ChildData := VirtualDrawTree1.GetNodeData(WorkNode);

     If ChildData <> Nil Then
     Begin
        If ChildData.WorkFlag = -1 Then
        Begin
           If FUpdateThreadCnt < FUpdateThreadMax Then
           Begin
              TUpdateThread.Create(ChildData.Images, ChildData.FullPath, VirtualDrawTree1.DefaultNodeHeight);
              // UpdateImage(ChildData.Images, ChildData.FullPath, VirtualDrawTree1.DefaultNodeHeight);

              // ChildData.Image.LoadFromFile('C:\Project\Component\VirtualTreeview V5.3.0\Demos\Advanced\Res\Cyrillic.bmp');
              ChildData.WorkFlag := 0;

              Updated := True;
              Break;
           End;
        End;
     End;

     WorkNode := VirtualDrawTree1.GetNext(WorkNode);
     If WorkNode = Nil Then Exit;
  End;

  If Updated Then
     VirtualDrawTree1.Invalidate;
end;

procedure TMainform.VirtualDrawTree1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PShellObjectData;
  LoopVar: Integer;
begin
  Data := Sender.GetNodeData(Node);
  If Assigned(Data.Image) Then Data.Image.Free;
  If Assigned(Data.Images) Then
  Begin
     For LoopVar := 0 to Data.Images.Count-1 do
     Begin
        TTagedBitmap(Data.Images.Items[LoopVar]).Free;
     End;
     Data.Images.Free;
  End;
  Finalize(Data^); // Clear string data.
end;

procedure TMainform.FormCreate(Sender: TObject);
var
  SFI: TSHFileInfo;
  DC: HDC;
begin
  Randomize;

  FConfigFile := TStringList.Create;
  ConfigFilePath := ExtractFilePath(Application.ExeName) + Const_ConfigFileName;
  If FileExists(ConfigFilePath) Then
     FConfigFile.LoadFromFile(ConfigFilePath);

  FUpdateThreadMax := 10;
  FUpdateThreadCnt := 0;
  av_register_all();
  // avcodec_register_all();

  DC := GetDC(0);
  FScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0,DC);

  SystemImages.Handle := SHGetFileInfo('', 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SystemImages.ShareImages := True;

  FAutoUpdateThread := TAutoPlayThread.Create;
  FAutoUpdateThread.FItemHeight := VirtualDrawTree1.DefaultNodeHeight;

  // Load Config
  Menu_ConfRememberLastUsedPath.Checked := (FConfigFile.Values[Const_VarLastPath] = 'true');
  If Menu_ConfRememberLastUsedPath.Checked Then
     BrowerEditor.Text:= FConfigFile.Values[Const_VarRememberLastPath];
end;

procedure TMainform.VirtualDrawTree1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PShellObjectData;
begin
  if Column = 0 then
  begin
     Data := Sender.GetNodeData(Node);
     case Kind of
        ikNormal,
        ikSelected:
        begin
           if Sender.Expanded[Node] then
              ImageIndex := Data.OpenIndex
           else
              ImageIndex := Data.CloseIndex;
        end;
        ikOverlay:
        if (Data.Attributes and SFGAO_SHARE) <> 0 then
           ImageIndex := 0
        else
           if (Data.Attributes and SFGAO_LINK) <> 0 then
              ImageIndex := 1;
     end;
  end;
end;

procedure TMainform.WMUSER(var msg: TMessage);
var
  CurrentNode: PVirtualNode;
  Data: PShellObjectData;
begin
  Case msg.LParam of
     1: FUpdateThreadCnt := FUpdateThreadCnt + 1;
     2:
     Begin
        FUpdateThreadCnt := FUpdateThreadCnt - 1;
        // Caption := IntToStr(FUpdateThreadCnt);
     End;

     1000:
     Begin
        VirtualDrawTree1.Invalidate;
     End;
     2000:
     Begin
        // FAutoUpdateThread.CurrentImage
        CurrentNode := VirtualDrawTree1.GetFirstSelected();
        Data := VirtualDrawTree1.GetNodeData(CurrentNode);

        If (Not FAutoUpdateThread.Terminated) and (Data <> Nil) Then
        Try
           If (Data.FullPath = FAutoUpdateThread.TargetFile) and (Not FAutoUpdateThread.CurrentImage.Empty) Then
           Begin
              Data.Image.Assign(FAutoUpdateThread.CurrentImage);
              VirtualDrawTree1.Invalidate;
           End;
        Except
        End;
     End;
  End;
end;

procedure TMainform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ThreadSyncTime := 0;
  FAutoUpdateThread.Terminate;

  // Const_VarRememberLastPath: String = 'RememberLastPath';
  // Const_VarLastPath: String = 'LastPath';
  FConfigFile.Values[Const_VarLastPath] := BoolToStr[Menu_ConfRememberLastUsedPath.Checked];
  If Menu_ConfRememberLastUsedPath.Checked Then
     FConfigFile.Values[Const_VarRememberLastPath] := BrowerEditor.Text;
  FConfigFile.SaveToFile(ConfigFilePath);
end;

procedure TMainform.VirtualDrawTree1FocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PShellObjectData;
begin
  If Node <> Nil Then
  Begin
     Data := VirtualDrawTree1.GetNodeData(Node);
     If Data = Nil Then Exit;

     // 갱신 스레드
     If Data.Attributes = 0 Then
        FAutoUpdateThread.PrepareFile(Data.FullPath)
     Else
        FAutoUpdateThread.PrepareFile('');
  End;
  //
end;

procedure TMainform.VirtualDrawTree1DblClick(Sender: TObject);
var
  CurrentNode: PVirtualNode;
  Data: PShellObjectData;

  procedure ExecuteFileWithAsso(AFileName: WideString);
  var
    SEInfo: TShellExecuteInfo;
    ExitCode: DWORD;
    ExecuteFile, ParamString, StartInString: string;
  begin
    ExecuteFile   := AFileName;
    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(TShellExecuteInfo);
    with SEInfo do
    begin
      fMask        := 0;
      Wnd          := Application.Handle;
      lpFile       := PChar(ExecuteFile);
      lpParameters := Nil;
      lpDirectory  := Nil;
      nShow        := SW_SHOWNORMAL;
    end;
    ShellExecuteEx(@SEInfo);
  end;
begin
  FAutoUpdateThread.PrepareFile('');

  CurrentNode := VirtualDrawTree1.GetFirstSelected();
  Data := VirtualDrawTree1.GetNodeData(CurrentNode);

  If Data = Nil Then Exit;

  If Data.Attributes = 0 Then
     // ShellExecute(Handle, nil, PChar('"'+Data.FullPath + '"'), nil, nil, SW_SHOW)
     ExecuteFileWithAsso(Data.FullPath)
  Else
     BrowerEditor.Text := RelToAbs(Data.FullPath); // PathCanonicalize(Data.FullPath);

  // ShellExecute(0, 0, PChar(Data.FullPath), 0, 0, SW_SHOWNORMAL);
end;

procedure TMainform.VirtualDrawTree1NodeClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
var
  ItemRect: TRect;
  P: TPoint;
  Data: PShellObjectData;
  WorkBitmap: TTagedBitmap;
  TargetItemIdx: Integer;
begin
  If HitInfo.HitNode = Nil Then Exit;

  If (HitInfo.HitColumn = 1) { and (vsSelected in HitInfo.HitNode.States) } Then
  Begin
     ItemRect := VirtualDrawTree1.GetDisplayRect(HitInfo.HitNode, 1, True);
     P := VirtualDrawTree1.ScreenToClient(Mouse.CursorPos);

     Data := VirtualDrawTree1.GetNodeData(HitInfo.HitNode);
     If Data = Nil Then Exit;
     If Data.Attributes <> 0 Then Exit;

     if Assigned(Data.Image) and (Not Data.Image.Empty) then
     Begin
        TargetItemIdx := (P.X - ItemRect.Left) div (Data.Image.Width + 2);

        If Data.Images.Count > 0 Then
        Begin
           If (TargetItemIdx >= Data.Images.Count) or (TargetItemIdx < 0) Then
              TargetItemIdx := Data.Images.Count-1;

           WorkBitmap := TTagedBitmap(Data.Images.Items[TargetItemIdx]);
           FAutoUpdateThread.SeekRequest(WorkBitmap.Tag);
           // Caption := IntToStr(WorkBitmap.Tag);
        End;
     End;
  End;
end;

procedure TMainform.VirtualDrawTree1CompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PShellObjectData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  
  Result := 0;

  If (Data1 = nil) or (Data2 = nil) then Exit;

  Result := Data2.Attributes - Data1.Attributes;
  If Result <> 0 Then Exit;

  Result := CompareStr(Data1.Display, Data2.Display);
end;

procedure TMainform.FormDestroy(Sender: TObject);
begin
  FConfigFile.Free;
end;

procedure TMainform.FunctionButtonClick(Sender: TObject);
var
  Position: TPoint;
begin
  Position := ClientToScreen(Point(FunctionButton.Left, FunctionButton.Top));
  MainPopupMenu.Popup(Position.X, Position.Y+FunctionButton.Height);
end;

procedure TMainform.Info1Click(Sender: TObject);
begin
  MessageBox(Self.Handle, PChar('NS Thumb VIEW' + #13#10#13#10 +Const_VersionString), 'Program info', MB_OK or MB_ICONINFORMATION);
end;

end.

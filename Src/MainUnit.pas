unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, XPMan, MMSystem, pngimage, jpeg, JvGIF,

  {$if CompilerVersion <= 15}
  TntClasses, TntSysUtils,
  {$ifend}

  {$I headers.inc}

  ShellApi, ShlObj, ImgList, ComCtrls, VirtualTrees, JvToolEdit, JvExStdCtrls,
  JvButton, JvCtrls, JclFileUtils, JvExMask, JvExControls, JvXPCore, JclWin32,
  JvXPButtons, JvArrowButton, JkhFlatButton, Menus, JkhLabel, JkhTimer{, JwaShLWAPI};

type
  TTagedBitmap = class(TBitmap)
  private
  public
    Tag: Int64; // for image time.
    Timeinfo: String;

    procedure Assign(Source: TPersistent); override;
  end;

  TTagedList = class(TList)
  private
  public
    Tag: Integer; // for full time.
    LoadingInfo: Integer;
    // Timeinfo:
  end;

  // item update
  TUpdateVideoThread = class(TThread)
  private
    FTargetList: TTagedList;
    {$if CompilerVersion <= 15}
    FTargetFileName: WideString;
    {$else}
    FTargetFileName: String;
    {$ifend}
    FItemHeight: Integer;

    ImageBuffer: TTagedBitmap;

    procedure Execute; override;
  public
    TargetEtcInfo: String;

    constructor Create(const ATargetList: TTagedList; Const ATargetFileName: String; AItemHeight: Integer);
  end;

  TUpdatePictureThread = class(TThread)
  private
    FTargetImage: TTagedBitmap;
    {$if CompilerVersion <= 15}
    FTargetFileName: WideString;
    {$else}
    FTargetFileName: String;
    {$ifend}
    FItemHeight: Integer;

    procedure Execute; override;
  public
    PublicImage: TTagedBitmap;

    constructor Create(const ATargetImage: TTagedBitmap; Const ATargetFileName: String; AItemHeight: Integer);
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
    destructor Destroy; override;

    procedure PrepareFile(ATargetFile: String);
    procedure SeekRequest(ASeekTime: Int64);
  end;

  TMainform = class(TForm)
    ThumbViewList: TVirtualDrawTree;
    XPManifest1: TXPManifest;
    URLUpdateTimer: TTimer;
    ThumbUpdate: TTimer;
    SystemImages: TImageList;
    MainPanel1: TPanel;
    BaseIconList: TImageList;
    MainPopupMenu: TPopupMenu;
    Info1: TMenuItem;
    N1: TMenuItem;
    Config1: TMenuItem;
    Menu_ConfRememberLastUsedPath: TMenuItem;
    N2: TMenuItem;
    File1: TMenuItem;
    Savethumbnails1: TMenuItem;
    humbnailgab1: TMenuItem;
    Menu_TimeGap3Min: TMenuItem;
    Menu_TimeGap1Min: TMenuItem;
    Menu_TimeGap5Min: TMenuItem;
    Menu_TimeGap10Min: TMenuItem;
    Menu_TimeGapCustom: TMenuItem;
    Menu_ViewTimeline: TMenuItem;
    ThumbMakePanel: TPanel;
    SaveThumbnailButton: TJkhFlatButton;
    MakeGIFAnimation: TJkhFlatButton;
    URIPanel: TPanel;
    BrowerEditor: TJvDirectoryEdit;
    JkhLabel1: TJkhLabel;
    MenuPanel: TPanel;
    FunctionButton: TJkhFlatButton;
    ThumbMake: TJkhFlatButton;
    AlertPanel: TPanel;
    AlertPanelHideTimer: TJkhTimer;
    humbnailheight1: TMenuItem;
    Menu_100px: TMenuItem;
    Menu_200px: TMenuItem;
    Menu_300px: TMenuItem;
    Menu_custompx: TMenuItem;
    procedure BrowerEditorChange(Sender: TObject);
    procedure URLUpdateTimerTimer(Sender: TObject);
    procedure ThumbViewListDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure FormShow(Sender: TObject);
    procedure ThumbUpdateTimer(Sender: TObject);
    procedure ThumbViewListFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure ThumbViewListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ThumbViewListFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ThumbViewListDblClick(Sender: TObject);
    procedure ThumbViewListNodeClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure ThumbViewListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure FunctionButtonClick(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure Menu_TimeGapCustomClick(Sender: TObject);
    procedure ThumbMakeClick(Sender: TObject);
    procedure SaveThumbnailButtonClick(Sender: TObject);
    procedure AlertPanelHideTimerTimer(Sender: TObject);
    procedure ThumbViewListKeyPress(Sender: TObject; var Key: Char);
    procedure Menu_SelectPxClick(Sender: TObject);
  private
    FScreenLogPixels: Integer;
    FAutoUpdateThread: TAutoPlayThread;
    FCurrentPath: String;
    FUpdateThreadCnt: Integer;
    FUpdateThreadMax: Integer;
    FUpdateMaxMemory: Cardinal;
    FTargetVideoFileExts: String;
    FTargetPictureFileExts: String;

    FConfigFile: TStringList;

    procedure WMUSER(var msg: TMessage); message WM_USER;

    procedure PanelAlert(const Msg: String);
  public
    procedure MakeVDTList(Target: WideString; ForceUpdate: Boolean = False);
  end;

const
  Const_VersionString: String = 'Version 0.1';

  Const_ConfigFileName: String = 'config.ini';
  Const_VarRememberLastPath: String = 'RememberLastPath';
  Const_VarLastPath: String = 'LastPath';
  Const_ThumbnailTimeGap: String = 'ThumbnailTimeGap';
  Const_ThumbnailHeight: String = 'ThumbnailHeight';
  Const_TargetVideoFileExts: String = 'TargetVideoFileExtension';
  Const_TargetPictureFileExts: String = 'TargetPictureFileExtension';
  Const_ViewTimeline: String = 'ViewTimeline';

  Default_TargetVideoFileExts = '.mkv;.mp4;.m4a;.m4v;.f4v;.f4a;.m4b;.m4r;.f4b;.mov;.3gp;.3gp2;.3g2;.3gpp;.3gpp2;.ogg;.oga;.ogv;.ogx;.wmv;.wma;.asf;.webm;.flv;.avi;.ogm;';
  Default_TargetPictureFileExts = '.gif;.png;.jpg;.jpeg;.bmp;.tif;.tiff;.ico;.emf;.wmf;';
  Default_ThumbViewWidth = 1000;
  Default_TimelineHeight = 9;
  Default_TimelineImageGap = 2;

  BoolToStr: Array[Boolean] of String = ('false', 'true');

var
  Mainform: TMainform;
  ThreadSyncTime: Longword;
  ConfigFilePath: String;
  ThumbnailTimeGap: Integer = 180000000;

implementation

uses SaveThumbnailUnit;

type
  // This data record contains all necessary information about a particular file system object.
  // This can either be a folder (virtual or real) or an image file.
  PShellObjectData = ^TShellObjectData;
  TShellObjectData = record
    FullPath,
    Display,
    EtcInfo: UnicodeString;
    WorkFlag: Integer;
    ObjectType: Integer; // -1 : none, 1: Folder, 10: Video, 11: Picture
    Attributes: Cardinal; // 0 : file, 1: folder
    OpenIndex,
    CloseIndex: Integer;      // image indices into the system image list
    Image: TTagedBitmap;
    Images: TTagedList;
    Properties: UnicodeString;   // some image properties, preformatted
  end;

{$R *.dfm}

function MemoryUsed: cardinal;
var
  st: TMemoryManagerState;
  sb: TSmallBlockTypeState;
begin
  GetMemoryManagerState(st);
  result := st.TotalAllocatedMediumBlockSize + st.TotalAllocatedLargeBlockSize;
  for sb in st.SmallBlockTypeStates do begin
     result := result + sb.UseableBlockSize * sb.AllocatedBlockCount;
  end;
end;

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

function ParentPath(const CurrentPath: WideString): WideString;
begin
  if Length(CurrentPath) <= 3 then
  begin
     Result := CurrentPath;
     Exit;
  end;

  Result := ExtractFilePath(ExcludeTrailingPathDelimiter(CurrentPath))
end;

procedure DivMod(Dividend: Integer; Divisor: Word;
  var Result, Remainder: Word);
asm
  PUSH    EBX
  MOV     EBX,EDX
  MOV     EDX,EAX
  SHR     EDX,16
  DIV     BX
  MOV     EBX,Remainder
  MOV     [ECX],AX
  MOV     [EBX],DX
  POP     EBX
end;

function MsToTimeStr(AInput: Longword): String;
var
  MinCount, MSecCount,
  Hour, Min, Sec, MSec: Word;
  OptionVar           : Byte;
Begin
  DivMod(AInput, 60000, MinCount, MSecCount);
  DivMod(MinCount, 60, Hour, Min);
  DivMod(MSecCount, 1000, Sec, MSec);

  Result := Format('%.2d:%.2d:%.2d.%d', [ Hour, Min, Sec, MSec ]);
end;

{ TTagedBitmap }

procedure TTagedBitmap.Assign(Source: TPersistent);
begin
  inherited;

  If Source is TTagedBitmap Then
  Begin
     Self.Tag := TTagedBitmap(Source).Tag;
     Self.Timeinfo := TTagedBitmap(Source).Timeinfo;
  End;
end;

{ TTagedList }

{ TUpdateThread }

constructor TUpdateVideoThread.Create(const ATargetList: TTagedList;
  Const ATargetFileName: String; AItemHeight: Integer);
begin
  SendMessage(Mainform.Handle, WM_USER, 0, 1);

  FTargetList := ATargetList;
  FTargetFileName := ATargetFileName;
  FItemHeight := AItemHeight;

  Inherited Create(False);

  Self.FreeOnTerminate := True;
end;

procedure TUpdateVideoThread.Execute;
const
  randomTimeZone = 60000000;
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
  MainformHandle: THandle;

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
      if ThreadSyncTime = 0 then Exit;

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
       if ThreadSyncTime = 0 then Break;

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
  MainformHandle := Mainform.Handle;
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

  src_filename := PAnsiChar(UTF8Encode(FTargetFileName));

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
     if fmt_ctx.duration_estimation_method = AVFMT_DURATION_FROM_PTS Then
     begin

     end;
     fullLength := fmt_ctx.duration;
     // Caption := IntToStr(fullLength);
     if Assigned(FTargetList) then
        FTargetList.Tag := fullLength;
     FTargetList.LoadingInfo := -1;

     If fullLength > 0 Then
     Begin
        RequestThumb := 100;
        TimeDivVar := ThumbnailTimeGap;
        If TimeDivVar > fullLength Then
           TimeDivVar := fullLength;
        SeekTime := 0;

        // Must read first stream.
        BuildFrame;

        If (fullLength > 1000) then
        Begin
           SeekCandidate := TimeDivVar - Random(randomTimeZone);
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
           if ThreadSyncTime = 0 then Break;

           If Terminated Then Break;
           
           If LoopVar > 1 Then
           Begin
              // av_seek_frame(fmt_ctx, video_stream_idx, SeekTime, AVSEEK_FLAG_BYTE); // AVSEEK_FLAG_ANY);
              SeekCandidate := SeekTime + Random(randomTimeZone);
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
              TempBitmap.Timeinfo := '';
              if (video_stream.first_dts <> AV_NOPTS_VALUE) and (video_stream.cur_dts <> AV_NOPTS_VALUE) then
                 TempBitmap.Timeinfo := MsToTimeStr( Round(av_q2d(video_stream.time_base) * video_stream.cur_dts * 1000) ); // av_ts2timestr(video_stream.cur_dts, @video_stream.time_base);
              //

              // 동기화가 되어 있을때만 처리한다. 아니면 바로 종료시킬것임
              // 다른 쓰레드는 역으로 SendMessage 에서 가져가지만 이쪽은 동시성의 처리를 수행하므로 이쪽에서 푸쉬한다.
              // 또 Free 도 저쪽에서 하므로 이쪽에서 Free 하지 않음.
              If KeepSyncTime = ThreadSyncTime Then
              Begin
                 FTargetList.Add(TempBitmap);
                 SendMessage(Mainform.Handle, WM_USER, 0, 1000);
              End
              Else
              Begin
                 TempBitmap.Free;
                 Break;
              End;
           End;

           SeekTime := SeekTime + TimeDivVar;

           if SeekTime >= fullLength then Break;
           If ImageBuffer.Empty Then Break;
        End;

        // TargetEtcInfo := av_ts2timestr(fullLength, @(video_stream.time_base));
     End;

     SendMessage(MainformHandle, WM_USER, 0, 1000);
     // TargeTTagedBitmap.Assign(ImageBuffer);
     // ImageBuffer
  Finally
     ImageBuffer.Free;

     SendMessage(MainformHandle, WM_USER, 0, 2);

     avcodec_free_context(@video_dec_ctx);
     avformat_close_input(@fmt_ctx);
     av_frame_free(@frame);
     // Result := Ord(ret < 0);

     if ThreadSyncTime <> 0 then
        FTargetList.LoadingInfo := 0;
  End;
end;

{ TUpdatePictureThread }

constructor TUpdatePictureThread.Create(const ATargetImage: TTagedBitmap;
  const ATargetFileName: String; AItemHeight: Integer);
begin
  SendMessage(Mainform.Handle, WM_USER, 0, 1);

  FTargetImage := ATargetImage;
  FTargetFileName := ATargetFileName;
  FItemHeight := AItemHeight;

  Inherited Create(False);

  Self.FreeOnTerminate := True;
end;

procedure TUpdatePictureThread.Execute;
var
  MainformHandle: THandle;
  GraphicLoader: TPicture;
  KeepSyncTime: Longword;
begin
  KeepSyncTime := ThreadSyncTime;

  MainformHandle := Mainform.Handle;
  if not Assigned(FTargetImage) then Exit;

  GraphicLoader := TPicture.Create;
  PublicImage := TTagedBitmap.Create;
  Try
     if Not FileExists(FTargetFileName) then Exit;

     Try
        GraphicLoader.LoadFromFile(FTargetFileName);

        PublicImage.Height := FItemHeight;
        PublicImage.Width := Round( (FItemHeight * GraphicLoader.Width) / GraphicLoader.Height );
        PublicImage.Canvas.StretchDraw(Rect(0,0, PublicImage.Width, PublicImage.Height), GraphicLoader.Graphic);

        if (ThreadSyncTime = 0) or (KeepSyncTime <> ThreadSyncTime) then Exit;
        FTargetImage.Assign(PublicImage);

        SendMessage(MainformHandle, WM_USER, 0, 1000);
     Except
     End;
  Finally
     GraphicLoader.Free;
     PublicImage.Free;

     SendMessage(MainformHandle, WM_USER, 0, 2);
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
  CurrentImage.Free;

  //     CurrentImage: TTagedBitmap;
  inherited;
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
  IsFirstTime: Boolean;

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

  src_filename := PAnsiChar(UTF8Encode(FTargetFileName));

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

     IsFirstTime := True;
     If fullLength > 0 Then
     Begin
        EstiTime := 0;
        While FTargetFileName = TargetFile do
        Begin
           If Self.Terminated Then Break;

           If (SeekRequestTime >= 0) and (Not IsFirstTime) Then
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
           IsFirstTime := False;

           If EstiTime > 500 Then
           Begin
              ConvertFlag := True; // 다음
           End;

           If Not ImageBuffer.Empty Then
           Begin
              CurrentImage.Assign(ImageBuffer);
              CurrentImage.Tag := video_stream.cur_dts;
              if (video_stream.first_dts <> AV_NOPTS_VALUE) and (video_stream.cur_dts <> AV_NOPTS_VALUE) then
                 CurrentImage.Timeinfo := MsToTimeStr( Round(av_q2d(video_stream.time_base) * video_stream.cur_dts * 1000) ); // av_ts2timestr(video_stream.cur_dts, @video_stream.time_base);

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
     {$if CompilerVersion <= 15}
     If (Not WideFileExists(UTF8Decode(TargetFile))) or (ProcessedFile = TargetFile) Then
     {$else}
     If (Not FileExists(TargetFile)) or (ProcessedFile = TargetFile) Then
     {$ifend}
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

procedure TMainform.AlertPanelHideTimerTimer(Sender: TObject);
begin
  AlertPanel.Visible := False;
end;

procedure TMainform.BrowerEditorChange(Sender: TObject);
begin
  URLUpdateTimer.Enabled := True;
end;

procedure TMainform.MakeVDTList(Target: WideString; ForceUpdate: Boolean);
var
  // Data,
  ChildData: PShellObjectData;
  SR: TSearchRec;
  TotalHeight: Integer;
  ChildNode: PVirtualNode;
  NewName: WideString;
  ParsedExt: String;
  TargetState: DWORD;

  function CheckFileAttributes(const Name: WideString): DWORD;
  begin
    Result := GetFileAttributesW(PWideChar(Name));
  end;

  function GetFullPath(Name: String): String;
  var
    ShortPath: array[0..MAX_PATH] Of Char;
    strLongPath: String;
  begin
    strLongPath := Name;
    GetShortPathName(pChar(strLongPath), ShortPath, SizeOf(ShortPath));
    Result := ShortPath;
  end;
begin
  If Not DirectoryExists(Target) Then Exit;
  If (Not ForceUpdate) and (Target = FCurrentPath) Then Exit;

  TotalHeight := 0;
  FCurrentPath := Target;

  // init width
  ThumbViewList.Header.Columns[1].MaxWidth := Default_ThumbViewWidth;
  ThumbViewList.Header.Columns[1].Width := Default_ThumbViewWidth;

  ThreadSyncTime := TimeGetTime();
  FAutoUpdateThread.PrepareFile('');

  ThumbViewList.NodeDataSize := SizeOf(TShellObjectData);
  ThumbViewList.RootNodeCount := 0;

  ThumbViewList.BeginUpdate;
  {$if CompilerVersion <= 15}
  if WideFindFirst(WideIncludeTrailingBackslash(Target) + '*.*', faAnyFile, SR) = 0 then
  {$else}
  if FindFirst(IncludeTrailingBackslash(Target) + '*.*', faAnyFile, SR) = 0 then
  {$ifend}
  begin
     Screen.Cursor := crHourGlass;
     Try
        repeat
           if (SR.Name <> '.') and (SR.Name <> '..') and (SR.Attr and faDirectory = 0) then
           begin
              {$if CompilerVersion <= 15}
              ParsedExt := WideExtractFileExt(SR.Name);
              {$else}
              ParsedExt := ExtractFileExt(SR.Name);
              {$ifend}
              If ParsedExt = '' then Continue;

              // Add Target Files.
              if Pos(LowerCase(ParsedExt)+';', FTargetVideoFileExts) > 0 Then
              begin
                 ChildNode := ThumbViewList.AddChild( ThumbViewList.RootNode );
                 if Menu_ViewTimeline.Checked then
                    ChildNode.NodeHeight := ThumbViewList.DefaultNodeHeight + Default_TimelineHeight
                 else
                    ChildNode.NodeHeight := ThumbViewList.DefaultNodeHeight;
                 TotalHeight := TotalHeight + ChildNode.NodeHeight;

                 ChildData := ThumbViewList.GetNodeData(ChildNode);

                 If ChildData = Nil Then Continue;

                 ChildData.Display := SR.Name;
                 ChildData.EtcInfo := '';
                 {$if CompilerVersion <= 15}
                 NewName := WideIncludeTrailingBackslash(Target) + SR.Name;
                 {$else}
                 NewName := IncludeTrailingBackslash(Target) + SR.Name;
                 {$ifend}
                 ChildData.FullPath := NewName;
                 ChildData.WorkFlag := -1;
                 ChildData.Image := TTagedBitmap.Create;
                 ChildData.Images := TTagedList.Create;
                 GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex, ChildData.CloseIndex);

                 ChildData.Attributes := 0;
                 ChildData.ObjectType := 10;
              end
              else if Pos(LowerCase(ParsedExt)+';', FTargetPictureFileExts) > 0 then
              begin
                 ChildNode := ThumbViewList.AddChild( ThumbViewList.RootNode );
                 ChildNode.NodeHeight := ThumbViewList.DefaultNodeHeight;
                 TotalHeight := TotalHeight + ChildNode.NodeHeight;

                 ChildData := ThumbViewList.GetNodeData(ChildNode);

                 If ChildData = Nil Then Continue;

                 ChildData.Display := SR.Name;
                 ChildData.EtcInfo := '';
                 {$if CompilerVersion <= 15}
                 NewName := WideIncludeTrailingBackslash(Target) + SR.Name;
                 {$else}
                 NewName := IncludeTrailingBackslash(Target) + SR.Name;
                 {$ifend}
                 ChildData.FullPath := NewName;
                 ChildData.WorkFlag := -1;
                 ChildData.Image := TTagedBitmap.Create;
                 ChildData.Images := TTagedList.Create;
                 GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex, ChildData.CloseIndex);

                 ChildData.Attributes := 0; // ReadAttributes(NewName);
                 ChildData.ObjectType := 11;
              end;

              (*
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
              *)
           end
           Else If (SR.Attr and faDirectory <> 0) and (not (SR.Name = '.')) then
           Begin
              {$if CompilerVersion <= 15}
              NewName := WideIncludeTrailingBackslash(Target) + SR.Name;
              {$else}
              NewName := IncludeTrailingBackslash(Target) + SR.Name;
              {$ifend}

              // not complete.
              TargetState := CheckFileAttributes(NewName);
              If (TargetState <> DWORD(-1)) and ((TargetState and FILE_ATTRIBUTE_REPARSE_POINT) <> 0) Then Continue;

              If (TargetState <> DWORD(-1)) and ((TargetState and FILE_ATTRIBUTE_DIRECTORY) <> 0) Then
              Begin
                 // ChildNode := ThumbViewList.InsertNode(Nil, amAddChildFirst);
                 ChildNode := ThumbViewList.AddChild( ThumbViewList.RootNode );

                 ChildNode.NodeHeight := Canvas.TextHeight('A') + 2;
                 TotalHeight := TotalHeight + ChildNode.NodeHeight;

                 ChildData := ThumbViewList.GetNodeData(ChildNode);
                 If ChildData = Nil Then Continue;

                 ChildData.Display := SR.Name;
                 ChildData.EtcInfo := '';
                 ChildData.FullPath := NewName;
                 ChildData.WorkFlag := 0;
                 ChildData.Image := TTagedBitmap.Create;
                 ChildData.Images := TTagedList.Create;
                 GetOpenAndClosedIcons(ChildData.FullPath, ChildData.OpenIndex, ChildData.CloseIndex);

                 ChildData.Attributes := 1;
                 ChildData.ObjectType := 1;
              End;
           End;
        {$if CompilerVersion <= 15}
        until WideFindNext(SR) <> 0;
        {$else}
        until FindNext(SR) <> 0;
        {$ifend}
     Finally
        ThumbViewList.SortTree(ThumbViewList.Header.SortColumn, ThumbViewList.Header.SortDirection);
        ThumbViewList.EndUpdate;

        {$if CompilerVersion <= 15}
        WideFindClose(SR);
        {$else}
        FindClose(SR);
        {$ifend}

        ThumbViewList.RootNode.NodeHeight := 0;
        ThumbViewList.RootNode.TotalHeight := TotalHeight;
        ThumbViewList.UpdateScrollBars(False);

        Screen.Cursor := crDefault;
     End;
  end;
end;

procedure TMainform.URLUpdateTimerTimer(Sender: TObject);
begin
  URLUpdateTimer.Enabled := False;

  MakeVDTList(BrowerEditor.Text);
end;

procedure TMainform.ThumbViewListDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var
  Data: PShellObjectData;
  WorkBitmap: TTagedBitmap;
  LoopMax, StartIdx,
  X: Integer;
  S: UnicodeString;
  R: TRect;
  lf: LOGFONT;
  PositionTime, PositionIndex,
  LastIndex: Integer;

  function TimeToStrConv(AInputTime: Longword): String;
  var
    dt : TDateTime;
  begin
    dt := AInputTime / 1000 / SecsPerDay;
    Result := FormatDateTime('hh:nn:ss', dt);
  end;

  procedure DrawVideo;
  var
    LoopVar: Integer;
  begin
    if Assigned(Data.Image) and (Not Data.Image.Empty) then
    Begin
       // BitBlt(Canvas.Handle, X, ContentRect.Top, Data.Image.Width, Data.Image.Height, Data.Image.Canvas.Handle, 0, 0, SRCCOPY);
       // X := X + Data.Image.Width + 2;
       // StartIdx := 1;
       PositionTime := Data.Image.Tag;
       PositionIndex := StartIdx;
    end;

    WorkBitmap := Nil;

    with Sender as TVirtualDrawTree, PaintInfo do
    Begin
       If Assigned(Data.Images) and (Data.Images.Count > 0) Then
       Begin
          LoopMax := Data.Images.Count-1;

          // timeline enabled?
          if Menu_ViewTimeline.Checked and (LoopMax>0) then
          begin
             Canvas.Pen.Color := clBlack;
             WorkBitmap := TTagedBitmap(Data.Images.Items[0]);
             Canvas.Rectangle(ContentRect.Left, ContentRect.Bottom-Default_TimelineHeight, ContentRect.Left+( (WorkBitmap.Width+Default_TimelineImageGap)*(LoopMax+1) )-Default_TimelineImageGap, ContentRect.Bottom);
          end;

          //If LoopMax >= 3 Then LoopMax := 2;
          For LoopVar := StartIdx to LoopMax do
          Begin
             WorkBitmap := TTagedBitmap(Data.Images.Items[LoopVar]);
             If (PositionTime > 0) and (PositionTime >= WorkBitmap.Tag) Then
                PositionIndex := LoopVar;

             // 영역 안의것만 처리하자.
             If ((X + WorkBitmap.Width) > 0) and (X < R.Right) Then
             begin
                BitBlt(Canvas.Handle, X, ContentRect.Top, WorkBitmap.Width, WorkBitmap.Height, WorkBitmap.Canvas.Handle, 0, 0, SRCCOPY);
                if Menu_ViewTimeline.Checked then
                begin
                   Canvas.Rectangle(X, WorkBitmap.Height, X+1, WorkBitmap.Height+Default_TimelineHeight);
                   // Canvas.Font.Color := clRed;
                   Canvas.Font.Size := Default_TimelineHeight-3;
                   Canvas.TextOut(X+1, WorkBitmap.Height-1, WorkBitmap.TimeInfo);
                end;
             end;

             X := X + WorkBitmap.Width + Default_TimelineImageGap;

             // fixed 1 column
             If ThumbViewList.Header.Columns[1].Width < X Then
             Begin
                ThumbViewList.Header.Columns[1].MaxWidth := X;
                ThumbViewList.Header.Columns[1].Width := X;
             End;
          End;

          if (Data.Images.LoadingInfo <> 0) and (WorkBitmap <> nil) then
             BaseIconList.Draw(Canvas, X+(WorkBitmap.Width div 2)-(BaseIconList.Width div 2), (WorkBitmap.Height div 2)-(BaseIconList.Height div 2), 1)
          // Else
          //   BaseIconList.Draw(Canvas, X + 100, (Node.NodeHeight div 2)-(BaseIconList.Height div 2), 1);
       End;

       If PositionIndex >= 0 Then
       Begin
          BitBlt(Canvas.Handle, ContentRect.Left+(PositionIndex*(Data.Image.Width+Default_TimelineImageGap)), ContentRect.Top, Data.Image.Width, Data.Image.Height, Data.Image.Canvas.Handle, 0, 0, SRCCOPY);

          if Menu_ViewTimeline.Checked then
          begin
             X := ContentRect.Left+(PositionIndex*(Data.Image.Width+2));
             // X := ContentRect.Right - ContentRect.Left;

             if Assigned(WorkBitmap) then
             begin
                Canvas.Pen.Color := clRed;
                // Canvas.Font.Color := clRed; // not good.
                Canvas.Font.Size := Default_TimelineHeight-3;
                Canvas.Rectangle(X+1, WorkBitmap.Height, X+WorkBitmap.Width, WorkBitmap.Height+Default_TimelineHeight);
                Canvas.TextOut(X+1, WorkBitmap.Height-1, Data.Image.Timeinfo);
             end;
          end;
       End;
    End;
  end;

  procedure DrawPicture;
  begin
    with Sender as TVirtualDrawTree, PaintInfo do
    if Assigned(Data.Image) and (Not Data.Image.Empty) then
    Begin
       BitBlt(Canvas.Handle, X, ContentRect.Top, Data.Image.Width, Data.Image.Height, Data.Image.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end;
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
           if Data.ObjectType = 10 Then
              DrawVideo
           else if Data.ObjectType = 11 Then
              DrawPicture;
        end;
     end;
  end;
end;

procedure TMainform.FormShow(Sender: TObject);
begin
  MakeVDTList(BrowerEditor.Text);
end;

procedure TMainform.ThumbMakeClick(Sender: TObject);
begin
  ThumbMake.Down := not ThumbMake.Down;
  ThumbMakePanel.Visible := ThumbMake.Down;
end;

procedure TMainform.ThumbUpdateTimer(Sender: TObject);
var
  LoopVar: Integer;
  WorkNode: PVirtualNode;
  ChildData: PShellObjectData;
  Updated: Boolean;
begin
  Updated := False;
  WorkNode := ThumbViewList.GetFirst();
  If WorkNode = Nil Then Exit;

  While True do
  Begin
     if Not ThumbUpdate.Enabled then Exit;

     ChildData := ThumbViewList.GetNodeData(WorkNode);

     If ChildData <> Nil Then
     Begin
        If ChildData.WorkFlag = -1 Then
        Begin
           If (FUpdateThreadCnt < FUpdateThreadMax) and (MemoryUsed < FUpdateMaxMemory) Then
           Begin
              if ChildData.ObjectType = 10 then
              begin
                 {$if CompilerVersion <= 15}
                 TUpdateVideoThread.Create(ChildData.Images, UTF8Encode(ChildData.FullPath), ThumbViewList.DefaultNodeHeight);
                 {$else}
                 TUpdateVideoThread.Create(ChildData.Images, ChildData.FullPath, ThumbViewList.DefaultNodeHeight);
                 {$ifend}
                 // UpdateImage(ChildData.Images, ChildData.FullPath, ThumbViewList.DefaultNodeHeight);
              end
              else if ChildData.ObjectType = 11 then
              begin
                 TUpdatePictureThread.Create(ChildData.Image, ChildData.FullPath, ThumbViewList.DefaultNodeHeight);
              end;

              // ChildData.Image.LoadFromFile('C:\Project\Component\VirtualTreeview V5.3.0\Demos\Advanced\Res\Cyrillic.bmp');
              ChildData.WorkFlag := 0;

              Updated := True;
              Break;
           End;
        End;
     End;

     WorkNode := ThumbViewList.GetNext(WorkNode);
     If WorkNode = Nil Then Exit;
  End;

  If Updated Then
     ThumbViewList.Invalidate;
end;

procedure TMainform.ThumbViewListFreeNode(Sender: TBaseVirtualTree;
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
  ConfigParseInt: Integer;
  ConfigParseStr: String;
begin
  Randomize;

  FConfigFile := TStringList.Create;
  ConfigFilePath := ExtractFilePath(Application.ExeName) + Const_ConfigFileName;
  If FileExists(ConfigFilePath) Then
     FConfigFile.LoadFromFile(ConfigFilePath);

  FUpdateThreadMax := 10;
  FUpdateMaxMemory := 300000000; // 500M limit
  FUpdateThreadCnt := 0;
  av_register_all();
  // avcodec_register_all();

  DC := GetDC(0);
  FScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0,DC);

  SystemImages.Handle := SHGetFileInfo('', 0, SFI, SizeOf(SFI), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SystemImages.ShareImages := True;

  FAutoUpdateThread := TAutoPlayThread.Create;
  FAutoUpdateThread.FItemHeight := ThumbViewList.DefaultNodeHeight;

  // Load Config
  Menu_ConfRememberLastUsedPath.Checked := (FConfigFile.Values[Const_VarLastPath] = 'true');
  If Menu_ConfRememberLastUsedPath.Checked Then
     BrowerEditor.Text:= FConfigFile.Values[Const_VarRememberLastPath];

  Menu_ViewTimeline.Checked := (FConfigFile.Values[Const_ViewTimeline] = 'true');

  ConfigParseStr := FConfigFile.Values[Const_ThumbnailTimeGap];
  If ConfigParseStr <> '' Then
  Begin
     TryStrToInt(ConfigParseStr, ThumbnailTimeGap);
     If ThumbnailTimeGap = 60000000 Then
        Menu_TimeGap1Min.Checked := True
     Else If ThumbnailTimeGap = 180000000 Then
        Menu_TimeGap3Min.Checked := True
     Else If ThumbnailTimeGap = 300000000 Then
        Menu_TimeGap5Min.Checked := True
     Else If ThumbnailTimeGap = 1000000000 Then
        Menu_TimeGap10Min.Checked := True
     Else
        Menu_TimeGapCustom.Checked := True;
  End;

  ConfigParseStr := FConfigFile.Values[Const_ThumbnailHeight];
  If ConfigParseStr <> '' Then
  Begin
     TryStrToInt(ConfigParseStr, ConfigParseInt);
     ThumbViewList.DefaultNodeHeight := ConfigParseInt;
     If ConfigParseInt = 100 Then
        Menu_100px.Checked := True
     Else If ConfigParseInt = 200 Then
        Menu_200px.Checked := True
     Else If ConfigParseInt = 300 Then
        Menu_300px.Checked := True
     Else
        Menu_CustomPx.Checked := True;
  End;

  FTargetVideoFileExts := Trim(FConfigFile.Values[Const_TargetVideoFileExts]);
  FTargetPictureFileExts := Trim(FConfigFile.Values[Const_TargetPictureFileExts]);
  If FTargetVideoFileExts = '' Then
     FTargetVideoFileExts := Default_TargetVideoFileExts;
  if FTargetPictureFileExts = '' then
     FTargetPictureFileExts := Default_TargetPictureFileExts;
end;

procedure TMainform.ThumbViewListGetImageIndex(Sender: TBaseVirtualTree;
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

procedure TMainform.ThumbViewListKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
     #8: BrowerEditor.Text := ParentPath(BrowerEditor.Text);
     #13: ThumbViewListDblClick(Sender);
  else
     Exit;
  end;

  Key := #0;
end;

procedure TMainform.WMUSER(var msg: TMessage);
var
  CurrentNode: PVirtualNode;
  Data: PShellObjectData;
begin
  If ThreadSyncTime = 0 Then Exit;

  Case msg.LParam of
     1: FUpdateThreadCnt := FUpdateThreadCnt + 1;
     2:
     Begin
        FUpdateThreadCnt := FUpdateThreadCnt - 1;
        // Caption := IntToStr(FUpdateThreadCnt);
     End;

     1000:
     Begin
        ThumbViewList.Invalidate;
     End;
     2000:
     Begin
        // FAutoUpdateThread.CurrentImage
        CurrentNode := ThumbViewList.GetFirstSelected();
        Data := ThumbViewList.GetNodeData(CurrentNode);

        If (Not FAutoUpdateThread.Terminated) and (Data <> Nil) Then
        Try
           {$if CompilerVersion <= 15}
           If (UTF8Encode(Data.FullPath) = FAutoUpdateThread.TargetFile) and (Not FAutoUpdateThread.CurrentImage.Empty) Then
           {$else}
           If (Data.FullPath = FAutoUpdateThread.TargetFile) and (Not FAutoUpdateThread.CurrentImage.Empty) Then
           {$ifend}
           Begin
              Data.Image.Assign(FAutoUpdateThread.CurrentImage);
              ThumbViewList.Invalidate;
           End;
        Except
        End;
     End;
  End;
end;

procedure TMainform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ThreadSyncTime := 0;

  URLUpdateTimer.Enabled := False;
  ThumbUpdate.Enabled := False;

  // 여기까지 와서 Free 할때는 괜히 Except 안내기 위해서 정책을 바꾼다.
  FAutoUpdateThread.FreeOnTerminate := False;
  FAutoUpdateThread.Terminate;
  Try
     FAutoUpdateThread.WaitFor;
  Except
     // dont care.
  End;
  FAutoUpdateThread.Free;

  // Save configs.
  FConfigFile.Values[Const_VarLastPath] := BoolToStr[Menu_ConfRememberLastUsedPath.Checked];
  FConfigFile.Values[Const_ViewTimeline] := BoolToStr[Menu_ViewTimeline.Checked];

  If Menu_ConfRememberLastUsedPath.Checked Then
     FConfigFile.Values[Const_VarRememberLastPath] := BrowerEditor.Text;

  FConfigFile.Values[Const_ThumbnailTimeGap] := IntToStr(ThumbnailTimeGap);
  FConfigFile.Values[Const_ThumbnailHeight] := IntToStr(ThumbViewList.DefaultNodeHeight);

  FConfigFile.Values[Const_TargetVideoFileExts] := FTargetVideoFileExts;
  FConfigFile.Values[Const_TargetPictureFileExts] := FTargetPictureFileExts;

  FConfigFile.SaveToFile(ConfigFilePath);
end;

procedure TMainform.ThumbViewListFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  Data: PShellObjectData;
begin
  If Node <> Nil Then
  Begin
     Data := ThumbViewList.GetNodeData(Node);
     If Data = Nil Then Exit;

     // 갱신 스레드
     If Data.Attributes = 0 Then
     Begin
        FAutoUpdateThread.FItemHeight := ThumbViewList.DefaultNodeHeight;
        {$if CompilerVersion <= 15}
        FAutoUpdateThread.PrepareFile(UTF8Encode(Data.FullPath))
        {$else}
        FAutoUpdateThread.PrepareFile(Data.FullPath)
        {$ifend}
     End
     Else
        FAutoUpdateThread.PrepareFile('');
  End;
  //
end;

procedure TMainform.ThumbViewListDblClick(Sender: TObject);
var
  CurrentNode: PVirtualNode;
  Data: PShellObjectData;

  procedure ExecuteFileWithAsso(AFileName: WideString);
  var
    SEInfo: TShellExecuteInfo;
    ExitCode: DWORD;
    ExecuteFile, ParamString, StartInString: WideString;
  begin
    ExecuteFile   := AFileName;
    FillChar(SEInfo, SizeOf(SEInfo), 0);
    SEInfo.cbSize := SizeOf(TShellExecuteInfoW);
    with SEInfo do
    begin
      fMask        := 0;
      Wnd          := Application.Handle;
      lpFile       := PChar(ExecuteFile);
      lpParameters := Nil;
      lpDirectory  := Nil;
      nShow        := SW_SHOWNORMAL;
    end;
    ShellExecuteExW(@SEInfo);
  end;
begin
  FAutoUpdateThread.PrepareFile('');

  CurrentNode := ThumbViewList.GetFirstSelected();
  Data := ThumbViewList.GetNodeData(CurrentNode);

  If Data = Nil Then Exit;

  If Data.ObjectType >= 10 Then
     // ShellExecute(Handle, nil, PChar('"'+Data.FullPath + '"'), nil, nil, SW_SHOW)
     ExecuteFileWithAsso(Data.FullPath)
  Else if Data.ObjectType = 1 then
     BrowerEditor.Text := RelToAbs(Data.FullPath); // PathCanonicalize(Data.FullPath);

  // ShellExecute(0, 0, PChar(Data.FullPath), 0, 0, SW_SHOWNORMAL);
end;

procedure TMainform.ThumbViewListNodeClick(Sender: TBaseVirtualTree;
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
     ItemRect := ThumbViewList.GetDisplayRect(HitInfo.HitNode, 1, True);
     P := ThumbViewList.ScreenToClient(Mouse.CursorPos);

     Data := ThumbViewList.GetNodeData(HitInfo.HitNode);
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

procedure TMainform.ThumbViewListCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PShellObjectData;
const
  SortDirectionToValueD: Array[TSortDirection] of Integer = (-1, 1);
  SortDirectionToValueR: Array[TSortDirection] of Integer = (1, -1);
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  Result := 0;

  If (Data1 = nil) or (Data2 = nil) then Exit;

  case Column of
     0:
     begin
        Result := Data2.Attributes - Data1.Attributes;

        if (Data1.Display = '..') then
           Result := SortDirectionToValueD[ThumbViewList.Header.SortDirection]
        else if (Data2.Display = '..') then
           Result := SortDirectionToValueR[ThumbViewList.Header.SortDirection];

        if Result <> 0 Then Exit;

        Result := CompareStr(Data1.Display, Data2.Display);
     end;
     1:
     begin
     end;
  end;

end;

procedure TMainform.FormDestroy(Sender: TObject);
begin
  FConfigFile.Free;
end;

procedure TMainform.FunctionButtonClick(Sender: TObject);
var
  Position: TPoint;
begin
  Position := ClientToScreen(Point(MenuPanel.Left+FunctionButton.Left, FunctionButton.Top));
  MainPopupMenu.Popup(Position.X, Position.Y+FunctionButton.Height);
end;

procedure TMainform.Info1Click(Sender: TObject);
begin
  MessageBox(Self.Handle, PChar('NS Thumb VIEW' + #13#10#13#10 + Const_VersionString), 'Program info', MB_OK or MB_ICONINFORMATION);
end;

procedure TMainform.Menu_SelectPxClick(Sender: TObject);
begin
  ThumbViewList.DefaultNodeHeight := (Sender as TMenuItem).Tag;
  MakeVDTList(BrowerEditor.Text, True);
end;

procedure TMainform.Menu_TimeGapCustomClick(Sender: TObject);
begin
  ThumbnailTimeGap := (Sender as TMenuItem).Tag;
  MakeVDTList(BrowerEditor.Text, True);
end;

procedure TMainform.PanelAlert(const Msg: String);
begin
  AlertPanel.Caption := ' ' + Msg;
  AlertPanel.Visible := True;
  AlertPanelHideTimer.Enabled := True;
end;

procedure TMainform.SaveThumbnailButtonClick(Sender: TObject);
var
  SelectedNode: PVirtualNode;
  SelectedList: TList;
  Data: PShellObjectData;
begin
  SelectedList := TList.Create;
  try
     SelectedNode := ThumbViewList.GetFirstSelected;
     if SelectedNode <> nil then
        while true do
        begin
           Data := ThumbViewList.GetNodeData(SelectedNode);
           if (Data <> Nil) and (Data.ObjectType >= 10) then
              SelectedList.Add(SelectedNode);

           SelectedNode := ThumbViewList.GetNextSelected(SelectedNode);
           if SelectedNode = nil then break;           
        end;

     if SelectedList.Count <= 0 then
     begin
        PanelAlert('No media file selected.');
        Exit;
     end;

     SaveThumbnailForm.ShowModal;

     case SaveThumbnailForm.Tag of
        1:
        begin
        end;
        2:
        begin
        end;
        3:
        begin
        end;
     end;
  finally
     SelectedList.Free;
  end;
end;

end.

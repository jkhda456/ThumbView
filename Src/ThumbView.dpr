program ThumbView;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {Mainform},
  SaveThumbnailUnit in 'SaveThumbnailUnit.pas' {SaveThumbnailForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Thumb VIEW';
  Application.CreateForm(TMainform, Mainform);
  Application.CreateForm(TSaveThumbnailForm, SaveThumbnailForm);
  Application.Run;
end.

program ThumbView;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {Mainform};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Thumb VIEW';
  Application.CreateForm(TMainform, Mainform);
  Application.Run;
end.

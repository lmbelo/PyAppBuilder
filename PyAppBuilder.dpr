program PyAppBuilder;

uses
  System.StartUpCopy,
  FMX.Forms,
  Form.Main in 'source\views\Form.Main.pas' {MainForm},
  From.Project in 'source\views\From.Project.pas' {ProjectForm},
  Form.Environment in 'source\views\Form.Environment.pas' {EnvironmentForm},
  Container.Images in 'source\containers\Container.Images.pas' {ImageContainer: TDataModule},
  Model.Environment in 'source\models\Model.Environment.pas',
  Model.Project in 'source\models\Model.Project.pas',
  Storage in 'source\storage\Storage.pas',
  Storage.Json in 'source\storage\Storage.Json.pas',
  Storage.Default in 'source\storage\Storage.Default.pas',
  Form.Data in 'source\views\Form.Data.pas' {DataForm},
  Form.Factory in 'source\views\Form.Factory.pas',
  Form.Slider in 'source\views\Form.Slider.pas',
  Services.IDE in 'source\services\Services.IDE.pas',
  {$IFDEF MSWINDOWS}
  Services.ADB.Win in 'source\services\Services.ADB.Win.pas',
  {$ELSE}
  Services.ADB.Posix in 'source\services\Services.ADB.Posix.pas',
  {$ENDIF }
  Services.ADB in 'source\services\Services.ADB.pas',
  Model in 'source\models\Model.pas',
  Form.Base in 'source\views\Form.Base.pas' {BaseForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TImageContainer, ImageContainer);
  Application.CreateForm(TBaseForm, BaseForm);
  Application.Run;
end.

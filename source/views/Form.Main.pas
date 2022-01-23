unit Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ListBox,
  FMX.StdCtrls, FMX.TabControl, System.Actions, FMX.ActnList, FMX.Ani,
  FMX.Objects, Form.Base, Services, Storage.Factory, Storage.Default,
  Model.Project, Model.Environment;

type
  TMainForm = class(TBaseForm, IServices, ILogServices)
    loEditor: TLayout;
    mmEditor: TMemo;
    pnlLeftMenu: TPanel;
    lbLeftMenu: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem3: TListBoxItem;
    tbScripts: TTabControl;
    tiMainScript: TTabItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem2: TListBoxItem;
    loDevice: TLayout;
    aiDevice: TAniIndicator;
    cbDevice: TComboBox;
    btnRefreshDevice: TSpeedButton;
    tbUpperMenu: TToolBar;
    loFooter: TLayout;
    spLog: TSplitter;
    rrSpliterGrip: TRoundRect;
    mmLog: TMemo;
    procedure ListBoxItem1Click(Sender: TObject);
    procedure ListBoxItem3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshDeviceClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxItem2Click(Sender: TObject);
  private
    procedure LoadDevices();
  public
    procedure Log(const AString: string);
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Threading,
  Container.Images, Form.Factory, Form.Slider, Services.Factory, Services.ADB;

{$R *.fmx}

procedure TMainForm.btnRefreshDeviceClick(Sender: TObject);
begin
  LoadDevices();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GlobalServices := Self;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GlobalServices := nil;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  LoadDevices();
end;

procedure TMainForm.ListBoxItem1Click(Sender: TObject);
begin
  var LForm := TFormSimpleFactory.CreateEnvironment();
  try
    TFormSlider.ShowModal(Self, LForm);
  finally
    LForm.Free();
  end;
end;

procedure TMainForm.ListBoxItem2Click(Sender: TObject);
begin
  inherited;
  var LAppService := TServiceSimpleFactory.CreateApp();
  var LProjectStorage := TDefaultStorage<TProjectModel>.Make();
  var LProjectModel: TProjectModel := nil;
  var LEnvironmentStorage := TDefaultStorage<TEnvironmentModel>.Make();
  var LEnvironmentModel: TEnvironmentModel := nil;

  if not LProjectStorage.LoadModel(LProjectModel) then
    raise Exception.Create('Application defs not set.');

  if not LEnvironmentStorage.LoadModel(LEnvironmentModel) then
    raise Exception.Create('Environment defs not set.');

  LAppService.CopyAppFiles(LProjectModel);
  LAppService.UpdateManifest(LProjectModel);
  LAppService.BuildApk(LProjectModel, LEnvironmentModel);
  LAppService.InstallApk(LProjectModel, LEnvironmentModel);

  var LAdbService := TServiceSimpleFactory.CreateAdb();
  var LResult := TStringList.Create();
  try
    LAdbService.RunApp(LEnvironmentModel.AdbLocation, LProjectModel.PackageName, LResult);
  finally
    LResult.Free();
  end;
end;

procedure TMainForm.ListBoxItem3Click(Sender: TObject);
begin
  var LForm := TFormSimpleFactory.CreateProject();
  try
    TFormSlider.ShowModal(Self, LForm);
  finally
    LForm.Free();
  end;
end;

procedure TMainForm.LoadDevices;
begin
  cbDevice.Clear();
  aiDevice.Enabled := true;
  aiDevice.Visible := true;
  btnRefreshDevice.Enabled := false;
  TTask.Run(procedure begin
    var LDevices := TStringList.Create();
    try
      var LStorage := TStorageSimpleFactory.CreateEnvironment();
      try
        var LService := TServiceSimpleFactory.CreateAdb();
        var LAdbPath := LStorage.GetAdbPath();

        if not LAdbPath.IsEmpty() then
          LService.ListDevices(LAdbPath, LDevices);
      finally
        TThread.Synchronize(nil, procedure begin
          cbDevice.Items.Text := LDevices.Text;
          if (cbDevice.Count > 0)  then
            cbDevice.ItemIndex := 0;

          aiDevice.Enabled := false;
          aiDevice.Visible := false;
          btnRefreshDevice.Enabled := true;
        end);
      end;
    finally
      LDevices.Free();
    end;
  end);
end;

procedure TMainForm.Log(const AString: string);
begin
  TThread.Synchronize(nil, procedure begin
    mmLog.Lines.Add(AString);
    mmLog.GoToLineEnd();
  end);
end;

end.

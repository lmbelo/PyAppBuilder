unit Form.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts, FMX.ListBox,
  FMX.StdCtrls, FMX.TabControl, System.Actions, FMX.ActnList, FMX.Ani,
  FMX.Objects, Form.Base, Services.IDE;

type
  TMainForm = class(TBaseForm, IServices, ILogServices, IADBServices)
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
  private
    FADBServices: IADBServices;
    procedure LoadDevices();
  public
    procedure Log(const AString: string);

    property ADBServices: IADBServices read FADBServices implements IADBServices;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.Threading,
  Container.Images, Form.Factory, Form.Slider, Services.ADB;

{$R *.fmx}

procedure TMainForm.btnRefreshDeviceClick(Sender: TObject);
begin
  LoadDevices();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GlobalServices := Self;
  FADBServices := TADBService.Create();
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FADBServices := nil;
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
      try
        FADBServices.ListDevices(LDevices);
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
  end);
end;

end.
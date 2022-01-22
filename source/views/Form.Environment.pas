unit Form.Environment;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts,
  FMX.ImgList, System.Actions, FMX.ActnList, Form.Data, Model.Environment,
  FMX.Ani;

type
  [Entity(TEnvironmentModel)]
  TEnvironmentForm = class(TDataForm)
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    edtSdkBasePath: TEdit;
    ListBoxItem2: TListBoxItem;
    edtApkSignerLocation: TEdit;
    ListBoxItem3: TListBoxItem;
    edtAdbLocation: TEdit;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem4: TListBoxItem;
    edtPythonDistBasePath: TEdit;
  protected
    procedure FormUpdate(); override;
    procedure ModelUpdate(); override;
  end;

var
  EnvironmentForm: TEnvironmentForm;

implementation

uses
  Container.Images;

{$R *.fmx}

{ TEnvironmentForm }

procedure TEnvironmentForm.FormUpdate;
begin
  with Model as TEnvironmentModel do begin
    edtSdkBasePath.Text := SdkBasePath;
    edtApkSignerLocation.Text := ApkSignerLocation;
    edtAdbLocation.Text := AdbLocation;
    edtPythonDistBasePath.Text := PythonDistBasePath;
  end;
end;

procedure TEnvironmentForm.ModelUpdate;
begin
  with Model as TEnvironmentModel do begin
    SdkBasePath := edtSdkBasePath.Text;
    ApkSignerLocation := edtApkSignerLocation.Text;
    AdbLocation := edtAdbLocation.Text;
    PythonDistBasePath := edtPythonDistBasePath.Text;
  end;
end;

end.
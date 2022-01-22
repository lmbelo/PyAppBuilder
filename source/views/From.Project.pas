unit From.Project;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects,
  FMX.Layouts, FMX.Edit, FMX.ListBox, FMX.ImgList, System.Actions,
  FMX.ActnList, Form.Data, Model.Project, FMX.Ani;

type
  [Entity(TProjectModel)]
  TProjectForm = class(TDataForm)
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    edtPackageName: TEdit;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem2: TListBoxItem;
    edtVersionCode: TEdit;
    ListBoxItem3: TListBoxItem;
    edtVersionName: TEdit;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem4: TListBoxItem;
    cbPythonVersion: TComboBox;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    ListBoxItem5: TListBoxItem;
    cbArchitecture: TComboBox;
  protected
    procedure FormUpdate(); override;
    procedure ModelUpdate(); override;
  end;

var
  ProjectForm: TProjectForm;

implementation

uses
  Container.Images;

{$R *.fmx}

{ TProjectForm }

procedure TProjectForm.FormUpdate;
const
  PY_VER: array[0..2] of TPythonVersion = (cp38, cp39, cp310);
  ARCH: array[0..1] of TArchitecture = (arm, aarch64);
begin
  with Model as TProjectModel do begin
    PackageName := edtPackageName.Text;
    VersionCode := edtVersionCode.Text.ToInteger();
    VersionName := edtVersionName.Text;
    PythonVersion := PY_VER[cbPythonVersion.ItemIndex];
    Architecture := ARCH[cbArchitecture.ItemIndex];
  end;
end;

procedure TProjectForm.ModelUpdate;
const
  PY_VER: array[cp38..cp310] of integer = (0, 1, 2);
  ARCH: array[arm..aarch64] of integer = (0, 1);
begin
  with Model as TProjectModel do begin
    edtPackageName.Text := PackageName;
    edtVersionCode.Text := VersionCode.ToString();
    edtVersionName.Text := VersionName;
    cbPythonVersion.ItemIndex := PY_VER[PythonVersion];
    cbArchitecture.ItemIndex := ARCH[Architecture];
  end;
end;

end.
unit frmColorPicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Colors, FMX.Controls.Presentation;

type
  TColorPickerFrm = class(TForm)
    ColorPicker1: TColorPicker;
    Button2: TButton;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    class function ShowMe(const ASource: TAlphaColor; out AResult: TAlphaColor): Boolean;
  end;

var
  ColorPickerFrm: TColorPickerFrm;

implementation

{$R *.fmx}

{ TColorPickerFrm }

procedure TColorPickerFrm.Button1Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrOK;
end;

procedure TColorPickerFrm.Button2Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrCancel;
end;

class function TColorPickerFrm.ShowMe(const ASource: TAlphaColor;
  out AResult: TAlphaColor): Boolean;
var
  LForm: TColorPickerFrm;
begin
  LForm:= TColorPickerFrm.Create(nil);
  try
    LForm.ColorPicker1.Color:= ASource;
    Result:= LForm.ShowModal = mrOK;
    if Result then
      AResult:= LForm.ColorPicker1.Color;
  finally
    FreeAndNil(LForm);
  end;
end;

end.

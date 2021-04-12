unit frmTextEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TTextEditorFrm = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    class function ShowMe(const ASource: string; out AResult: string): Boolean;
  end;

var
  TextEditorFrm: TTextEditorFrm;

implementation

{$R *.fmx}

procedure TTextEditorFrm.Button1Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrOK;
end;

procedure TTextEditorFrm.Button2Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrCancel;
end;

class function TTextEditorFrm.ShowMe(const ASource: string;
  out AResult: string): Boolean;
var
  LForm: TTextEditorFrm;
begin
  LForm:= TTextEditorFrm.Create(nil);
  try
    LForm.Memo1.Text:= ASource;
    Result:= LForm.ShowModal = mrOK;
    if Result then
      AResult:= LForm.Memo1.Text;
  finally
    FreeAndNil(LForm);
  end;
end;

end.

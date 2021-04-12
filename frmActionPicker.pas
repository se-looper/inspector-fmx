unit frmActionPicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.EditBox, FMX.SpinBox, FMX.StdCtrls, FMX.Objects,
  FMX.Controls.Presentation, FMX.TreeView, FMX.Layouts, FMX.SearchBox,
  FMX.ListBox;

type
  TActionPickerFrm = class(TForm)
    Layout1: TLayout;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    Splitter1: TSplitter;
    Layout2: TLayout;
    Button2: TButton;
    Button1: TButton;
    ListBox1: TListBox;
    SearchBox1: TSearchBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    class function ShowMe(const ASource: string; out AResult: string): Boolean;
  end;

var
  ActionPickerFrm: TActionPickerFrm;

implementation

{$R *.fmx}

{ TActionPickerFrm }

procedure TActionPickerFrm.Button1Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrOK;
end;

procedure TActionPickerFrm.Button2Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrCancel;
end;

class function TActionPickerFrm.ShowMe(const ASource: string;
  out AResult: string): Boolean;
var
  LForm: TActionPickerFrm;
begin
  LForm:= TActionPickerFrm.Create(nil);
  try
    LForm.SearchBox1.Text:= ASource;
    Result:= LForm.ShowModal = mrOK;
    if Result then
      AResult:= LForm.ListBox1.Selected.Text;
  finally
    FreeAndNil(LForm);
  end;
end;

end.

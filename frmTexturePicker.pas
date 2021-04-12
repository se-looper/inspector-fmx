unit frmTexturePicker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.TreeView, FMX.Edit,
  FMX.EditBox, FMX.SpinBox;

type
  TTexturePickerFrm = class(TForm)
    TreeView1: TTreeView;
    Layout1: TLayout;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    PaintBox1: TPaintBox;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    Layout2: TLayout;
    Button2: TButton;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure Select(const ASource: string);
    function Selected: string;
  public
    class function ShowMe(const ASource: string; out AResult: string): Boolean;
  end;

var
  TexturePickerFrm: TTexturePickerFrm;

implementation

{$R *.fmx}

{ TTexturePickerFrm }

procedure TTexturePickerFrm.Button1Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrOK;
end;

procedure TTexturePickerFrm.Button2Click(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrCancel;
end;

procedure TTexturePickerFrm.Select(const ASource: string);
begin

end;

function TTexturePickerFrm.Selected: string;
begin
  Result:= 'start start_noticebtn';  //test
end;

class function TTexturePickerFrm.ShowMe(const ASource: string;
  out AResult: string): Boolean;
var
  LForm: TTexturePickerFrm;
begin
  LForm:= TTexturePickerFrm.Create(nil);
  try
    LForm.Select(ASource);
    Result:= LForm.ShowModal = mrOK;
    if Result then
      AResult:= LForm.Selected;
  finally
    FreeAndNil(LForm);
  end;
end;

end.

unit Editor.Inspector.PropEditor.Custom;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Types, System.UITypes,
  System.TypInfo, System.Generics.Collections, System.UIConsts,
  FMX.Controls, FMX.Types, FMX.Graphics, FMX.Objects, FMX.ListBox, FMX.Text,
  FMX.Layouts, FMX.StdCtrls, FMX.Edit, FMX.Memo,
  Editor.Inspector.PropEditor;

type
  TBooleanPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TAColorPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TImageFilePropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TStringListTextPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TActionNamePropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

implementation

uses
  frmTexturePicker, frmActionPicker, frmColorPicker, frmTextEditor;

{ TBooleanPropEditor }

procedure TBooleanPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LPropEditor: TControl;
begin
  LPropEditor:= TCheckBoxEx.Create(AItem);
  with LPropEditor do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 80;
  end;
  TCheckBoxEx(LPropEditor).OwnerObject:= AObject;
  TCheckBoxEx(LPropEditor).PropName:= AProp.Name;
  TCheckBoxEx(LPropEditor).IsChecked:= AProp.GetValue(AObject).AsBoolean;
  TCheckBoxEx(LPropEditor).Text:= '';
  TCheckBoxEx(LPropEditor).OnChange:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    begin
      System.TypInfo.SetPropValue(TCheckBoxEx(Sender).OwnerObject,
        TCheckBoxEx(Sender).PropName, TCheckBoxEx(Sender).IsChecked);
    end);
  AItem.Data:= LPropEditor;
end;

{ TAColorPropEditor }

procedure TAColorPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LLayout: TLayout;
  LEdit: TEdit;
  LPropEditor: TControl;
begin
  LLayout:= TLayout.Create(AItem);
  with LLayout do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 150;
    HitTest:= False;
  end;

  LEdit:= TEdit.Create(LLayout);
  with LEdit do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Client;
    Margins.Right:= 2;
    Width:= 120;
    ReadOnly:= True;
    Text:= AlphaColorToString(AProp.GetValue(AObject).AsType<TAlphaColor>);
  end;

  LPropEditor:= TDataButton.Create(LLayout);
  with LPropEditor do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Right;
    Width:= 25;
  end;
  AItem.Data:= LPropEditor;
  TDataButton(LPropEditor).OwnerObject:= AObject;
  TDataButton(LPropEditor).PropName:= AProp.Name;
  TDataButton(LPropEditor).DisplayControl:= LEdit;
  TDataButton(LPropEditor).RawData:= AProp.GetValue(AObject).AsType<TAlphaColor>;
  TDataButton(LPropEditor).Images:= FInspector.Images;
  TDataButton(LPropEditor).ImageIndex:= 42;
  TDataButton(LPropEditor).OnClick:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    var
      LPicked: TAlphaColor;
    begin
      if TColorPickerFrm.ShowMe(TDataButton(Sender).RawData.AsType<TAlphaColor>,
        LPicked) then
      begin
        System.TypInfo.SetPropValue(TDataButton(Sender).OwnerObject,
          TDataButton(Sender).PropName, LPicked);
        //
        if Assigned(TDataButton(LPropEditor).DisplayControl) then
          TEdit(TDataButton(LPropEditor).DisplayControl).Text:= AlphaColorToString(LPicked);
      end;
    end);
end;

{ TImageFilePropEditor }

procedure TImageFilePropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LLayout: TLayout;
  LEdit: TEdit;
  LPropEditor: TControl;
begin
  LLayout:= TLayout.Create(AItem);
  with LLayout do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 150;
    HitTest:= False;
  end;

  LEdit:= TEdit.Create(LLayout);
  with LEdit do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Client;
    Margins.Right:= 2;
    Width:= 120;
    ReadOnly:= True;
    Text:= AProp.GetValue(AObject).AsString;
  end;

  LPropEditor:= TDataButton.Create(LLayout);
  with LPropEditor do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Right;
    Width:= 25;
  end;
  AItem.Data:= LPropEditor;
  TDataButton(LPropEditor).OwnerObject:= AObject;
  TDataButton(LPropEditor).PropName:= AProp.Name;
  TDataButton(LPropEditor).DisplayControl:= LEdit;
  TDataButton(LPropEditor).RawData:= AProp.GetValue(AObject).AsString;
  TDataButton(LPropEditor).Images:= FInspector.Images;
  TDataButton(LPropEditor).ImageIndex:= 44;
  TDataButton(LPropEditor).OnClick:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    var
      LPicked: string;
    begin
      if TTexturePickerFrm.ShowMe(TDataButton(Sender).RawData.AsString, LPicked) then
      begin
        System.TypInfo.SetPropValue(TDataButton(Sender).OwnerObject,
          TDataButton(Sender).PropName, LPicked);
        //
        if Assigned(TDataButton(LPropEditor).DisplayControl) then
          TEdit(TDataButton(LPropEditor).DisplayControl).Text:= LPicked;
      end;
    end);
end;

{ TStringListTextPropEditor }

procedure TStringListTextPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LLayout: TLayout;
  LEdit: TMemo;
  LPropEditor: TControl;
begin
  LLayout:= TLayout.Create(AItem);
  with LLayout do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 150;
    HitTest:= False;
  end;

  LEdit:= TMemo.Create(LLayout);
  with LEdit do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Client;
    Margins.Right:= 2;
    Width:= 120;
    ReadOnly:= True;
    Lines.Text:= AProp.GetValue(AObject).AsString;
  end;

  LPropEditor:= TDataButton.Create(LLayout);
  with LPropEditor do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Right;
    Width:= 25;
  end;
  AItem.Data:= LPropEditor;
  TDataButton(LPropEditor).OwnerObject:= AObject;
  TDataButton(LPropEditor).PropName:= AProp.Name;
  TDataButton(LPropEditor).DisplayControl:= LEdit;
  TDataButton(LPropEditor).RawData:= AProp.GetValue(AObject).AsString;
  TDataButton(LPropEditor).Images:= FInspector.Images;
  TDataButton(LPropEditor).ImageIndex:= 43;
  TDataButton(LPropEditor).OnClick:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    var
      LResult: string;
    begin
      if TTextEditorFrm.ShowMe(TDataButton(Sender).RawData.AsString, LResult) then
      begin
        System.TypInfo.SetPropValue(TDataButton(Sender).OwnerObject,
          TDataButton(Sender).PropName, LResult);
        //
        if Assigned(TDataButton(LPropEditor).DisplayControl) then
          TMemo(TDataButton(LPropEditor).DisplayControl).Lines.Text:= LResult;
      end;
    end);
end;

{ TActionNamePropEditor }

procedure TActionNamePropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LLayout: TLayout;
  LEdit: TEdit;
  LPropEditor: TControl;
begin
  LLayout:= TLayout.Create(AItem);
  with LLayout do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 120;
    HitTest:= False;
  end;

  LEdit:= TEdit.Create(LLayout);
  with LEdit do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Client;
    Margins.Right:= 2;
    Width:= 90;
    ReadOnly:= True;
    Text:= AProp.GetValue(AObject).AsString;
  end;

  LPropEditor:= TDataButton.Create(LLayout);
  with LPropEditor do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Right;
    Width:= 25;
  end;
  AItem.Data:= LPropEditor;
  TDataButton(LPropEditor).OwnerObject:= AObject;
  TDataButton(LPropEditor).PropName:= AProp.Name;
  TDataButton(LPropEditor).DisplayControl:= LEdit;
  TDataButton(LPropEditor).RawData:= AProp.GetValue(AObject).AsString;
  TDataButton(LPropEditor).Images:= FInspector.Images;
  TDataButton(LPropEditor).ImageIndex:= 45;
  TDataButton(LPropEditor).OnClick:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    var
      LPicked: string;
    begin
      if TActionPickerFrm.ShowMe(TDataButton(Sender).RawData.AsString, LPicked) then
      begin
        System.TypInfo.SetPropValue(TDataButton(Sender).OwnerObject,
          TDataButton(Sender).PropName, LPicked);
        //
        if Assigned(TDataButton(LPropEditor).DisplayControl) then
          TEdit(TDataButton(LPropEditor).DisplayControl).Text:= LPicked;
      end;
    end);
end;

end.

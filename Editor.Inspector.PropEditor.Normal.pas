unit Editor.Inspector.PropEditor.Normal;

interface

uses
  System.Classes, System.SysUtils, System.Rtti, System.Types, System.UITypes,
  System.TypInfo, System.Generics.Collections,
  FMX.Controls, FMX.Types, FMX.Graphics, FMX.Objects, FMX.ListBox, FMX.Text,
  FMX.Layouts,
  Editor.Inspector.PropEditor;

type
  TIntegerPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TFloatPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TStringPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TEnumPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TClassPropEditor = class(TAbstractPropEditor)
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

  TEnumSetPropEditor = class(TAbstractPropEditor)
  private
    procedure MakeEnumSetItemPropEditor(const AOwnerObject: TObject;
      const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
  public
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); override;
  end;

implementation

{ TIntegerPropEditor }

procedure TIntegerPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LPropEditor: TControl;
begin
  LPropEditor:= TSpinBoxEx.Create(AItem);
  with LPropEditor do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 80;
  end;
  TSpinBoxEx(LPropEditor).OwnerObject:= AObject;
  TSpinBoxEx(LPropEditor).PropName:= AProp.Name;
  TSpinBoxEx(LPropEditor).Max:= MaxInt;
  TSpinBoxEx(LPropEditor).ValueType:= TNumValueType.Integer;
  TSpinBoxEx(LPropEditor).Value:= AProp.GetValue(AObject).AsInteger;
  TSpinBoxEx(LPropEditor).OnChange:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    begin
      System.TypInfo.SetPropValue(TSpinBoxEx(Sender).OwnerObject,
        TSpinBoxEx(Sender).PropName, Round(TSpinBoxEx(Sender).Value));
    end);
  AItem.Data:= LPropEditor;
end;

{ TFloatPropEditor }

procedure TFloatPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LPropEditor: TControl;
begin
  LPropEditor:= TSpinBoxEx.Create(AItem);
  with LPropEditor do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 120;
  end;
  TSpinBoxEx(LPropEditor).OwnerObject:= AObject;
  TSpinBoxEx(LPropEditor).PropName:= AProp.Name;
  TSpinBoxEx(LPropEditor).Max:= MaxInt;
  TSpinBoxEx(LPropEditor).ValueType:= TNumValueType.Float;
  TSpinBoxEx(LPropEditor).Value:= AProp.GetValue(AObject).AsExtended;
  TSpinBoxEx(LPropEditor).OnChange:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    begin
      System.TypInfo.SetPropValue(TSpinBoxEx(Sender).OwnerObject,
        TSpinBoxEx(Sender).PropName, TSpinBoxEx(Sender).Value);
    end);
  AItem.Data:= LPropEditor;
end;

{ TStringPropEditor }

procedure TStringPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LPropEditor: TControl;
begin
  LPropEditor:= TEditEx.Create(AItem);
  with LPropEditor do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 120;
  end;
  TEditEx(LPropEditor).OwnerObject:= AObject;
  TEditEx(LPropEditor).PropName:= AProp.Name;
  TEditEx(LPropEditor).Text:= AProp.GetValue(AObject).AsString;
  TEditEx(LPropEditor).OnChange:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    begin
      System.TypInfo.SetPropValue(TEditEx(Sender).OwnerObject,
        TEditEx(Sender).PropName, TEditEx(Sender).Text);
    end);
  AItem.Data:= LPropEditor;
end;

{ TEnumPropEditor }

procedure TEnumPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LPropEditor: TControl;
  LPropValue: TValue;
begin
  LPropEditor:= TComboBoxEx.Create(AItem);
  with LPropEditor do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= 120;
  end;
  LPropValue:= AProp.GetValue(AObject);
  TComboBoxEx(LPropEditor).OwnerObject:= AObject;
  TComboBoxEx(LPropEditor).PropName:= AProp.Name;
  //获取枚举名称列表
  TComboBoxEx(LPropEditor).Items.Text:= GetEnumNamesString(LPropValue.TypeInfo);
  //定位选择的枚举类型
  TComboBoxEx(LPropEditor).ItemIndex:= LPropValue.AsOrdinal;
  TComboBoxEx(LPropEditor).OnChange:= AnonProc2NotifyEvent(LPropEditor,
    procedure(Sender: TObject)
    begin
      System.TypInfo.SetPropValue(TComboBoxEx(Sender).OwnerObject,
        TComboBoxEx(Sender).PropName, TComboBoxEx(Sender).Selected.Text);
    end);
  AItem.Data:= LPropEditor;
end;

{ TClassPropEditor }

procedure TClassPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LLayout: TLayout;
  LText: TText;
begin
  LLayout:= TLayout.Create(AItem);
  with LLayout do
  begin
    Parent:= AItem;
    Align:= TAlignLayout.Right;
    Margins.Top:= cEditorTopSpace;
    Margins.Right:= cEditorRightSpace;
    Margins.Bottom:= cEditorBottomSpace;
    Width:= FInspector.Width;
    HitTest:= False;
  end;

  LText:= TText.Create(LLayout);
  with LText do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Client;
    Margins.Right:= 2;
    Text:= Format('(%s)',[AProp.PropertyType.Name]);
    TextSettings.HorzAlign:= TTextAlign.Trailing;
    HitTest:= False;
  end;
  if Assigned(FBaseCallback) then
    FBaseCallback(AProp.GetValue(AObject).AsType<TObject>, AItem);
end;

{ TEnumSetPropEditor }

procedure TEnumSetPropEditor.MakeEnumSetItemPropEditor(const AOwnerObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LItem: TCustomTreeViewItem;
  I: Integer;
  LEnumName: string;
  LEnumSetTypeInfo: PTypeInfo;
  LInstanceValue: TValue;
  LEnumSetItemPropEditor: TCheckBoxEx;
begin
  FInspector.BeginUpdate;
  try
    LInstanceValue:= AProp.GetValue(AOwnerObject);
    //获取集合的类型信息
    LEnumSetTypeInfo:= LInstanceValue.TypeData.CompType^;
    with System.TypInfo.GetTypeData(LEnumSetTypeInfo)^ do
    begin
      //遍历集合定义的所有枚举项
      for I:= MinValue to MaxValue do
      begin
        //枚举名称
        LEnumName:= System.TypInfo.GetEnumName(LEnumSetTypeInfo, I);
        //
        LItem:= Self.AddChildItem(AItem, LEnumName);
        //创建枚举编辑控件(TCheckBox)
        LEnumSetItemPropEditor:= TCheckBoxEx.Create(LItem);
        with LEnumSetItemPropEditor do
        begin
          Parent:= LItem;
          Align:= TAlignLayout.Right;
          Margins.Top:= cEditorTopSpace;
          Margins.Right:= cEditorRightSpace;
          Margins.Bottom:= cEditorBottomSpace;
          Width:= 80;
          Text:= '';
          //枚举是否在集合中
          IsChecked:= EnumInEnumSet(AOwnerObject, AProp.Name, I);
          //集合在对象中的属性名
          EnumSetPropName:= AProp.Name;
          //枚举名称
          PropName:= LEnumName;
          //所属对象
          OwnerObject:= AOwnerObject;
        end;
        LEnumSetItemPropEditor.OnChange:= AnonProc2NotifyEvent(LEnumSetItemPropEditor,
          procedure(Sender: TObject)
          begin
            EnumSetValueChange(TCheckBoxEx(Sender).OwnerObject,
                               TCheckBoxEx(Sender).EnumSetPropName,
                               TCheckBoxEx(Sender).PropName,
                               TCheckBoxEx(Sender).IsChecked);
          end);
      end;
    end;
  finally
    FInspector.EndUpdate;
  end;
end;

procedure TEnumSetPropEditor.MakeEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LLayout: TLayout;
  LText: TText;
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

  LText:= TText.Create(LLayout);
  with LText do
  begin
    Parent:= LLayout;
    Align:= TAlignLayout.Client;
    Margins.Right:= 2;
    Width:= 120;
    Text:= Format('[%s]',[AProp.PropertyType.Name]);
    TextSettings.HorzAlign:= TTextAlign.Trailing;
    HitTest:= False;
  end;
  MakeEnumSetItemPropEditor(AObject, AProp, AItem);
end;

end.

unit Editor.Inspector;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.Rtti,
  System.TypInfo, System.Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Objects, FMX.Controls, FMX.Edit,
  FMX.StdCtrls, FMX.Text, FMX.SpinBox, FMX.TreeView, FMX.ListBox,
  Editor.Inspector.PropEditor;

type
  TEditorInspector = class(TTreeView)
  private
    FNormalEditorClassMap: TDictionary<TTypeKind, TAbstractPropEditorClass>;
    FCustomEditorClassMap: TDictionary<string, TAbstractPropEditorClass>;
    FItemMap: TDictionary<string, TCustomTreeViewItem>;
    function AddItem(const AParent: TFmxObject;
      const AText: string): TCustomTreeViewItem;
    procedure MakePropEditorItems(const ASender: TObject; const AParent: TFmxObject);
  private
    procedure MakePropEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Select(const AObject: TObject);
    procedure Update(const APropName: string; const APropValue: TValue);

    procedure RegisterPropEditor(const ATypeKind: TTypeKind;
      const AEditorClass: TAbstractPropEditorClass); overload;
    procedure RegisterPropEditor(const ATypeName: string;
      const AEditorClass: TAbstractPropEditorClass); overload;
  end;

implementation

uses Editor.Inspector.PropEditor.Normal;

{ TEditorInspector }

function TEditorInspector.AddItem(const AParent: TFmxObject;
  const AText: string): TCustomTreeViewItem;
begin
  Result:= TCustomTreeViewItem.Create(AParent);
  Result.Parent:= AParent;
  Result.Text:= AText;
  //只有第一层的属性才根据属性名称关联Item
  if AParent is Self.ClassType then
    FItemMap.AddOrSetValue(AText, Result);
end;

constructor TEditorInspector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNormalEditorClassMap:= TDictionary<TTypeKind, TAbstractPropEditorClass>.Create;
  FCustomEditorClassMap:= TDictionary<string, TAbstractPropEditorClass>.Create;
  FItemMap:= TDictionary<string, TCustomTreeViewItem>.Create;
  //内置支持的属性编辑
  FNormalEditorClassMap.AddOrSetValue(tkInteger, TIntegerPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkFloat, TFloatPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkUString, TStringPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkLString, TStringPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkWString, TStringPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkString, TStringPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkEnumeration, TEnumPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkClass, TClassPropEditor);
  FNormalEditorClassMap.AddOrSetValue(tkSet, TEnumSetPropEditor);
  //
  Self.ItemHeight:= cItemHeight;
  Self.DisableFocusEffect:= False;
  Self.AlternatingRowBackground:= True;
end;

destructor TEditorInspector.Destroy;
begin
  FreeAndNil(FNormalEditorClassMap);
  FreeAndNil(FCustomEditorClassMap);
  FreeAndNil(FItemMap);
  inherited;
end;

procedure TEditorInspector.RegisterPropEditor(const ATypeKind: TTypeKind;
  const AEditorClass: TAbstractPropEditorClass);
begin
  FNormalEditorClassMap.AddOrSetValue(ATypeKind, AEditorClass);
end;

procedure TEditorInspector.RegisterPropEditor(const ATypeName: string;
  const AEditorClass: TAbstractPropEditorClass);
begin
  FCustomEditorClassMap.AddOrSetValue(ATypeName, AEditorClass);
end;

procedure TEditorInspector.MakePropEditor(const AObject: TObject;
  const AProp: TRttiProperty; const AItem: TCustomTreeViewItem);
var
  LClass: TAbstractPropEditorClass;
begin
  if not FCustomEditorClassMap.TryGetValue(AProp.PropertyType.Name, LClass) then
    if not FNormalEditorClassMap.TryGetValue(AProp.PropertyType.TypeKind, LClass) then
      Exit;
  //
  with LClass.Create(AItem) do
  begin
    Inspector:= Self;
    BaseCallback:= Self.MakePropEditorItems;
    MakeEditor(AObject, AProp, AItem);
  end;
end;

procedure TEditorInspector.MakePropEditorItems(const ASender: TObject; const AParent: TFmxObject);
var
  LComparer: TPropNameComparer;
  RTX: TRttiContext;
  RT: TRttiType;
  RP: TRttiProperty;
  RPS: TArray<TRttiProperty>;
begin
  LComparer:= TPropNameComparer.Create;
  Self.BeginUpdate;
  try
    RT:= RTX.GetType(ASender.ClassType);
    RPS:= RT.GetProperties;
    TArray.Sort<TRttiProperty>(RPS, LComparer);
    for RP in RPS do
      if (RP.Visibility = TMemberVisibility.mvPublished) and RP.IsWritable and
         (RP.PropertyType.TypeKind in [tkInteger, tkFloat,tkUString,tkLString,
          tkWString,tkString,tkEnumeration,tkClass,tkSet]) then
      begin
        Self.MakePropEditor(ASender, RP, Self.AddItem(AParent, RP.Name));
      end;
  finally
    FreeAndNil(LComparer);
    Self.EndUpdate;
  end;
end;

procedure TEditorInspector.Select(const AObject: TObject);
begin
  Self.Clear;
  if AObject = nil then Exit;
  Self.MakePropEditorItems(AObject, Self);
end;

procedure TEditorInspector.Update(const APropName: string;
  const APropValue: TValue);
var
  LItem: TCustomTreeViewItem;
begin
  if not FItemMap.TryGetValue(APropName, LItem) then
    Exit;
  if LItem.Data.IsType<TSpinBox> then
  begin
    case LItem.Data.AsType<TSpinBox>.ValueType of
      TNumValueType.Integer: LItem.Data.AsType<TSpinBox>.Value:= APropValue.AsInteger;
      TNumValueType.Float: LItem.Data.AsType<TSpinBox>.Value:= APropValue.AsExtended;
    end;
  end
  else
  if LItem.Data.IsType<TEdit> then
    LItem.Data.AsType<TEdit>.Text:= APropValue.AsString
  else
  if LItem.Data.IsType<TSwitch> then
    LItem.Data.AsType<TSwitch>.IsChecked:= APropValue.AsBoolean
  else
  if LItem.Data.IsType<TCheckBox> then
    LItem.Data.AsType<TCheckBox>.IsChecked:= APropValue.AsBoolean
  else
  if LItem.Data.IsType<TComboBox>  then
    LItem.Data.AsType<TComboBox>.ItemIndex:= APropValue.AsInteger;
end;

end.

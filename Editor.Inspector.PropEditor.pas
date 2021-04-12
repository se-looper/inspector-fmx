unit Editor.Inspector.PropEditor;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Rtti,
  System.TypInfo, System.Generics.Defaults,
  FMX.Types, FMX.ListBox, FMX.StdCtrls, FMX.Text, FMX.Edit, FMX.SpinBox,
  FMX.Layouts, FMX.Controls, FMX.TreeView;

type
  TCustomTreeViewItem = class(TTreeViewItem)
  private
    FOnExpandChanged: TNotifyEvent;
  protected
    FData: TValue;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure SetIsExpanded(const Value: Boolean); override;
  public
    property OnExpandChanged: TNotifyEvent read FOnExpandChanged write FOnExpandChanged;
  end;

  TBaseMakePropItemsCallbackEvent = procedure (const AObject: TObject;
    const AParent: TFmxObject) of object;

  TSpinBoxEx = class(TSpinBox)
  public
    OwnerObject: TObject;
    PropName: string;
  end;

  TEditEx = class(TEdit)
  public
    OwnerObject: TObject;
    PropName: string;
  end;

  TSwitchEx = class(TSwitch)
  public
    OwnerObject: TObject;
    PropName: string;
  end;

  TComboBoxEx = class(TComboBox)
  public
    OwnerObject: TObject;
    PropName: string;
  end;

  TCheckBoxEx = class(TCheckBox)
  public
    OwnerObject: TObject;
    PropName, EnumSetPropName: string;
  end;

  TDataButton = class(TSpeedButton)
  public
    OwnerObject: TObject;
    PropName: string;
    RawData: TValue;
    DisplayControl: TControl;
  end;

  TNotifyEventWrapper= class(TComponent)
  private
    FProc: TProc<TObject>;
  public
    constructor Create(Owner: TComponent; Proc: TProc<TObject>);
  published
    procedure Event(Sender: TObject);
  end;

  TPropNameComparer = class(TComparer<TRttiProperty>)
  public
    function Compare(const Left, Right: TRttiProperty): Integer; override;
  end;

  TAbstractPropEditor = class abstract(TComponent)
  protected
    FInspector: TTreeView;
    FBaseCallback: TBaseMakePropItemsCallbackEvent;
    function AddChildItem(const AParent: TFmxObject;
      const AText: string): TCustomTreeViewItem;
  public
    procedure MakeEditor(const AObject: TObject; const AProp: TRttiProperty;
      const AItem: TCustomTreeViewItem); virtual; abstract;

    property Inspector: TTreeView read FInspector write FInspector;
    property BaseCallback: TBaseMakePropItemsCallbackEvent read FBaseCallback write FBaseCallback;
  end;
  TAbstractPropEditorClass = class of TAbstractPropEditor;

function AnonProc2NotifyEvent(Owner: TComponent; Proc: TProc<TObject>): TNotifyEvent;
function GetEnumNames(const ATypeInfo: PTypeInfo): TArray<string>; overload;
function GetEnumNamesString(const ATypeInfo: PTypeInfo): string;
procedure EnumSetValueChange(const AObject: TObject; const AEnumSetName,
  AEnumName: string; const AInclude: Boolean);
function EnumInEnumSet(const AObject: TObject; const AEnumSetName: string;
  const AEnumValue: Byte): Boolean;

const
  cItemHeight = 28;
  cEditorTopSpace = 3;
  cEditorBottomSpace = 3;
  cEditorRightSpace = 5;

implementation

function AnonProc2NotifyEvent(Owner: TComponent; Proc: TProc<TObject>): TNotifyEvent;
begin
  Result:= TNotifyEventWrapper.Create(Owner, Proc).Event;
end;

function GetEnumNames(const ATypeInfo: PTypeInfo): TArray<string>;
var
  RTX: TRttiContext;
  RT: TRttiType;
  ROT: TRttiOrdinalType;
  I: Integer;
begin
  RT:= RTX.GetType(ATypeInfo);
  if RT.IsOrdinal then
  begin
    ROT:= RT.AsOrdinal;
    SetLength(Result, ROT.MaxValue + 1);
    for I:= ROT.MinValue to ROT.MaxValue do
      Result[I]:= System.TypInfo.GetEnumName(ATypeInfo, I);
  end;
end;

function GetEnumNamesString(const ATypeInfo: PTypeInfo): string;
var
  LStrings: TStrings;
  LNames: TArray<string>;
  LName: string;
begin
  LStrings:= TStringList.Create;
  try
    LNames:= GetEnumNames(ATypeInfo);
    for LName in LNames do
      LStrings.Add(LName);
    Result:= LStrings.Text;
  finally
    FreeAndNil(LStrings);
  end;
end;

procedure EnumSetValueChange(const AObject: TObject; const AEnumSetName,
  AEnumName: string; const AInclude: Boolean);
var
  LEnumSetPropInfo: PPropInfo;
  LEnumSetValue: Integer;
  LEnumValue: Integer;
begin
  LEnumSetPropInfo:= System.TypInfo.GetPropInfo(AObject, AEnumSetName);
  if LEnumSetPropInfo <> nil then
  begin
    LEnumSetValue:= System.TypInfo.GetOrdProp(AObject, LEnumSetPropInfo);
    LEnumValue:= System.TypInfo.GetEnumValue(
      System.TypInfo.GetTypeData(LEnumSetPropInfo^.PropType^)^.CompType^, AEnumName);
    if AInclude then
      TIntegerSet(LEnumSetValue):= TIntegerSet(LEnumSetValue) + [LEnumValue]
    else
      TIntegerSet(LEnumSetValue):= TIntegerSet(LEnumSetValue) - [LEnumValue];
  end;
  System.TypInfo.SetOrdProp(AObject, AEnumSetName, LEnumSetValue);
end;

function EnumInEnumSet(const AObject: TObject; const AEnumSetName: string;
  const AEnumValue: Byte): Boolean;
var
  LEnumSetPropInfo: PPropInfo;
  LEnumSetValue: Integer;
begin
  LEnumSetPropInfo:= System.TypInfo.GetPropInfo(AObject, AEnumSetName);
  LEnumSetValue:= System.TypInfo.GetOrdProp(AObject, LEnumSetPropInfo);
  Result:= (LEnumSetValue and (1 shl AEnumValue)) <> 0;
end;

{ TCustomTreeViewItem }

function TCustomTreeViewItem.GetData: TValue;
begin
  Result:= FData;
end;

procedure TCustomTreeViewItem.SetData(const Value: TValue);
begin
  FData:= Value;
end;

procedure TCustomTreeViewItem.SetIsExpanded(const Value: Boolean);
begin
  inherited;

end;

{ TNotifyEventWrapper }

constructor TNotifyEventWrapper.Create(Owner: TComponent;
  Proc: TProc<TObject>);
begin
  inherited Create(Owner);
  FProc:= Proc;
end;

procedure TNotifyEventWrapper.Event(Sender: TObject);
begin
  FProc(Sender);
end;

{ TPropNameComparer }

function TPropNameComparer.Compare(const Left, Right: TRttiProperty): Integer;
begin
  Result:= System.SysUtils.CompareStr(Left.Name, Right.Name);
end;

{ TAbstractPropEditor }

function TAbstractPropEditor.AddChildItem(const AParent: TFmxObject;
  const AText: string): TCustomTreeViewItem;
begin
  Result:= TCustomTreeViewItem.Create(AParent);
  Result.Parent:= AParent;
  Result.Text:= AText;
end;

end.

# inspector-fmx
FMX下通用可扩展的属性编辑器

基于TTreeView，相关代码：
1、构造示例
  FEditorInspector:= TEditorInspector.Create(lytLeft);
  FEditorInspector.Parent:= lytLeft;
  FEditorInspector.Align:= TAlignLayout.Client;
  FEditorInspector.Images:= ImageList1;
  //custom
  FEditorInspector.RegisterPropEditor('Boolean', TBooleanPropEditor);
  FEditorInspector.RegisterPropEditor('TAColor', TAColorPropEditor);
  FEditorInspector.RegisterPropEditor('TImageFileName', TImageFilePropEditor);
  FEditorInspector.RegisterPropEditor('TStringListText', TStringListTextPropEditor);
  FEditorInspector.RegisterPropEditor('TActionName', TActionNamePropEditor);
  
2、调用示例
procedure TMainFrm.pbxRendererMouseLeave(Sender: TObject);
begin
  FEditorInspector.Select(Sender);
end;

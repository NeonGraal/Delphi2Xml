@Echo Off
Set Range=%1
If "%1"=="" Set Range=Local
Delphi2Xml -mUses -pDir -gUsesXml -wx- @%Range%.prm

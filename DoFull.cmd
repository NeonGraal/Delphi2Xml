@Echo Off
Set Range=%1
If "%1"=="" Set Range=Local
Delphi2Xml -gFull -pDir @%Range%.prm

<?xml version="1.0"?>
<D2X_Param parseMode="Uses" fileName="Process Xml">
	<D2X_Dir fileName=".">
		<D2X_Pattern fileName="Pattern-TestingTest">
			<D2X_File fileName="Testing.TestUnit.pas">
				<ParseFile lastToken="end.">
					<MainUnitName>Testing.TestUnit</MainUnitName>
					<InterfaceSection>
						<UsedUnitName>System.Classes</UsedUnitName>
						<IncludeFile filename="TEST.INC" msgAt="68,0" />
						<D2X_notSuppMsg msgAt="88,0">Currently not supported {$D+}</D2X_notSuppMsg>
					</InterfaceSection>
					<ImplementationSection lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}">
						<UsedUnitName>System.SysUtils</UsedUnitName>
					</ImplementationSection>
				</ParseFile>
			</D2X_File>
			<D2X_File fileName="Testing.TestProgram.dpr">
				<ParseFile>
					<MainUnitName>Testing.TestProgram</MainUnitName>
					<ProgramBlock lastToken="beginend">
						<MainUsedUnitStatement>
							<UsedUnitName>Testing.TestUnit</UsedUnitName>
							<MainUsedUnitExpression file="'Testing.TestUnit.pas'" />
						</MainUsedUnitStatement>
					</ProgramBlock>
				</ParseFile>
			</D2X_File>
		</D2X_Pattern>
	</D2X_Dir>
</D2X_Param>
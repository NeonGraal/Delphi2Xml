<?xml version="1.0"?>
<D2X_Run parseMode="Uses" fileName="">
	<D2X_Param fileName="Process Xml">
		<D2X_Dir fileName=".">
			<D2X_Pattern fileName="Pattern-TestingTest">
				<D2X_File fileName="Testing.TestUnit.pas">
					<ParseFile lastToken="end.">
						<MainUnitName>
							<UnitName>Testing.TestUnit</UnitName>
						</MainUnitName>
						<InterfaceSection>
							<UsedUnitName>
								<UnitName>System.Classes</UnitName>
							</UsedUnitName>
							<IncludeFile filename="TEST.INC" msgAt="68,0" />
							<D2X_notSuppMsg msgAt="88,0">Currently not supported {$D+}</D2X_notSuppMsg>
						</InterfaceSection>
						<ImplementationSection lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}">
							<UsedUnitName>
								<UnitName>System.SysUtils</UnitName>
							</UsedUnitName>
						</ImplementationSection>
					</ParseFile>
				</D2X_File>
				<D2X_File fileName="Testing.TestProgram.dpr">
					<ParseFile>
						<MainUnitName>Testing.TestProgram</MainUnitName>
						<ProgramBlock lastToken="beginend">
							<MainUsedUnitStatement>
								<UsedUnitName>
									<UnitName>Testing.TestUnit</UnitName>
								</UsedUnitName>
								<MainUsedUnitExpression file="'Testing.TestUnit.pas'" />
							</MainUsedUnitStatement>
						</ProgramBlock>
					</ParseFile>
				</D2X_File>
			</D2X_Pattern>
		</D2X_Dir>
	</D2X_Param>
</D2X_Run>
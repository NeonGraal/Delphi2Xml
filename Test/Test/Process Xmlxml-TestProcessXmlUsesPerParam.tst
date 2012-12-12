<?xml version="1.0"?>
<D2X_Param parseMode="Uses" fileName="Process Xml">
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
					<MainUnitName>
						<UnitName>Testing.TestProgram</UnitName>
					</MainUnitName>
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
	<D2X_Dir fileName="Config">
		<D2X_Pattern fileName="ConfigPattern-TestingTest">
			<D2X_File fileName="Config\Testing.TestDir.pas">
				<ParseFile lastToken="end.">
					<MainUnitName>
						<UnitName>Testing.TestDir</UnitName>
					</MainUnitName>
					<InterfaceSection lastToken="interface" />
					<ImplementationSection lastToken="implementation" />
				</ParseFile>
			</D2X_File>
		</D2X_Pattern>
		<D2X_SubDir fileName="Config\Test">
			<D2X_Pattern fileName="Config\TestPattern-TestingTest">
				<D2X_File fileName="Config\Test\Testing.TestSubDir.pas">
					<ParseFile lastToken="end.">
						<MainUnitName>
							<UnitName>Testing.TestSubDir</UnitName>
						</MainUnitName>
						<InterfaceSection lastToken="interface" />
						<ImplementationSection lastToken="implementation" />
					</ParseFile>
				</D2X_File>
			</D2X_Pattern>
		</D2X_SubDir>
	</D2X_Dir>
	<D2X_Dir fileName="Test" />
</D2X_Param>
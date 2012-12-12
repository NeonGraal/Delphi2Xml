<?xml version="1.0"?>
<D2X_Run parseMode="Full" fileName="">
	<D2X_Param fileName="Process Xml">
		<D2X_Dir fileName=".">
			<D2X_Pattern fileName="Pattern-TestingTest">
				<D2X_File fileName="Testing.TestUnit.pas">
					<ParseFile>
						<UnitFile>
							<MainUnitName>
								<UnitName>Testing.TestUnit</UnitName>
							</MainUnitName>
							<InterfaceSection>
								<UsesClause>
									<UsedUnitsList>
										<UsedUnitName>
											<UnitName>System.Classes</UnitName>
										</UsedUnitName>
									</UsedUnitsList>
									<IncludeFile filename="TEST.INC" msgAt="68,0" />
									<D2X_notSuppMsg msgAt="88,0">Currently not supported {$D+}</D2X_notSuppMsg>
								</UsesClause>
							</InterfaceSection>
							<ImplementationSection>
								<UsesClause lastToken=";{$ELSE}{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}">
									<UsedUnitsList>
										<UsedUnitName>
											<UnitName>System.SysUtils</UnitName>
										</UsedUnitName>
									</UsedUnitsList>
								</UsesClause>
							</ImplementationSection>
							<InitializationSection lastToken="end" />
						</UnitFile>
					</ParseFile>
				</D2X_File>
				<D2X_File fileName="Testing.TestProgram.dpr">
					<ParseFile>
						<ProgramFile>
							<MainUnitName>
								<UnitName>Testing.TestProgram</UnitName>
							</MainUnitName>
							<ProgramBlock>
								<MainUsesClause>
									<MainUsedUnitStatement>
										<MainUsedUnitName>
											<UsedUnitName>
												<UnitName>Testing.TestUnit</UnitName>
											</UsedUnitName>
										</MainUsedUnitName>
										<MainUsedUnitExpression file="">
											<ConstantExpression>
												<Expression>
													<SimpleExpression>
														<Term>
															<Factor>
																<CharString lastToken="'Testing.TestUnit.pas'" />
															</Factor>
														</Term>
													</SimpleExpression>
												</Expression>
											</ConstantExpression>
										</MainUsedUnitExpression>
									</MainUsedUnitStatement>
								</MainUsesClause>
								<Block>
									<CompoundStatement lastToken="end">
										<StatementList />
									</CompoundStatement>
								</Block>
							</ProgramBlock>
						</ProgramFile>
					</ParseFile>
				</D2X_File>
			</D2X_Pattern>
		</D2X_Dir>
		<D2X_Dir fileName="Config">
			<D2X_Pattern fileName="ConfigPattern-TestingTest">
				<D2X_File fileName="Config\Testing.TestDir.pas">
					<ParseFile>
						<UnitFile>
							<MainUnitName>
								<UnitName>Testing.TestDir</UnitName>
							</MainUnitName>
							<InterfaceSection lastToken="interface" />
							<ImplementationSection lastToken="implementation" />
							<InitializationSection lastToken="end" />
						</UnitFile>
					</ParseFile>
				</D2X_File>
			</D2X_Pattern>
			<D2X_SubDir fileName="Config\Test">
				<D2X_Pattern fileName="Config\TestPattern-TestingTest">
					<D2X_File fileName="Config\Test\Testing.TestSubDir.pas">
						<ParseFile>
							<UnitFile>
								<MainUnitName>
									<UnitName>Testing.TestSubDir</UnitName>
								</MainUnitName>
								<InterfaceSection lastToken="interface" />
								<ImplementationSection lastToken="implementation" />
								<InitializationSection lastToken="end" />
							</UnitFile>
						</ParseFile>
					</D2X_File>
				</D2X_Pattern>
			</D2X_SubDir>
		</D2X_Dir>
		<D2X_Dir fileName="Test" />
	</D2X_Param>
</D2X_Run>
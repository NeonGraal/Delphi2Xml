<?xml version="1.0"?>
<D2X_Run parseMode="Full" fileName="">
	<D2X_Param fileName="Process Xml">
		<D2X_Dir fileName=".">
			<D2X_Pattern fileName="Pattern-TestingTest">
				<D2X_File fileName="Testing.TestUnit.pas">
					<ParseFile>
						<UnitFile>
							<MainUnitName>
								<UnitName lastToken="Testing.TestUnit" />
							</MainUnitName>
							<InterfaceSection>
								<UsesClause>
									<UsedUnitsList>
										<UsedUnitName>
											<UnitName lastToken="System.Classes" />
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
											<UnitName lastToken="System.SysUtils" />
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
								<UnitName lastToken="Testing.TestProgram" />
							</MainUnitName>
							<ProgramBlock>
								<MainUsesClause>
									<MainUsedUnitStatement>
										<MainUsedUnitName>
											<UsedUnitName>
												<UnitName lastToken="Testing.TestUnit" />
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
	</D2X_Param>
</D2X_Run>
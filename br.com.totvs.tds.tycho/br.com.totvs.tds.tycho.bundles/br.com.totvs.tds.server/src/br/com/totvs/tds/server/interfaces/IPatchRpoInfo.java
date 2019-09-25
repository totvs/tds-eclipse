package br.com.totvs.tds.server.interfaces;

import java.util.Map;

public interface IPatchRpoInfo {

	void addProgramAPP(String name, String date);

	Map<String, String> getAllPrograms();

	String getBuildFileApplication();

	String getBuildFileGerenation();

	String getDateFileApplication();

	String getDateFileGeneration();

	IRpoInfo getRpoInfo();

	boolean getSkipOld();

	int getTypePatch();

	void setAllPrograms(Map<String, String> allPrograms);

	void setBuildFileApplication(String buildFileApplication);

	void setBuildFileGerenation(String buildFileGerenation);

	void setDateFileApplication(String dateFileApplication);

	void setDateFileGeneration(String dateFileGeneration);

	void setSkipOld(boolean skipOld);

	void setTypePatch(int typePatch);

}
package br.com.totvs.tds.server.interfaces;

import java.util.List;

public interface IRpoInfo {

	void addRpoPatchs(IPatchRpoInfo patch);

	String getDateGeneration();

	String getEnvironment();

	List<IPatchRpoInfo> getRpoPatchs();

	String getRpoVersion();

	void setDateGeneration(String dateGeneration);

	void setEnvironment(String environment);

	void setRpoPatchs(List<IPatchRpoInfo> rpoPatchs);

	void setRpoVersion(String rpoVersion);

}
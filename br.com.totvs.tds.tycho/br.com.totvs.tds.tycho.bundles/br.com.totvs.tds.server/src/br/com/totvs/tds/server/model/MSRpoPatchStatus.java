package br.com.totvs.tds.server.model;

import java.util.HashMap;
import java.util.Map;

public final class MSRpoPatchStatus {

	private String buildFileApplication;

	private String buildFileGeneration;

	private int countPrograms;

	private String dateFileApplication;

	private String dateFileGeneration;

	private Map<String, String> programsApp = new HashMap<>();

	private boolean skipOld;

	private int typePatch;

	/**
	 * @return the buildFileApplication
	 */
	public String getBuildFileApplication() {
		return buildFileApplication;
	}

	/**
	 * @return the buildFileGeneration
	 */
	public String getBuildFileGeneration() {
		return buildFileGeneration;
	}

	/**
	 * @return the countPrograms
	 */
	public int getCountPrograms() {
		return countPrograms;
	}

	/**
	 * @return the dateFileApplication
	 */
	public String getDateFileApplication() {
		return dateFileApplication;
	}

	/**
	 * @return the dateFileGeneration
	 */
	public String getDateFileGeneration() {
		return dateFileGeneration;
	}

	/**
	 * @return the programsApp
	 */
	public Map<String, String> getProgramsApp() {
		return programsApp;
	}

	/**
	 * @return the typePatch
	 */
	public int getTypePatch() {
		return typePatch;
	}

	/**
	 * @return the skipOld
	 */
	public boolean isSkipOld() {
		return skipOld;
	}

	/**
	 * @param buildFileApplication
	 *            the buildFileApplication to set
	 */
	public void setBuildFileApplication(String buildFileApplication) {
		this.buildFileApplication = buildFileApplication;
	}

	/**
	 * @param buildFileGeneration
	 *            the buildFileGeneration to set
	 */
	public void setBuildFileGeneration(String buildFileGeneration) {
		this.buildFileGeneration = buildFileGeneration;
	}

	/**
	 * @param countPrograms
	 *            the countPrograms to set
	 */
	public void setCountPrograms(int countPrograms) {
		this.countPrograms = countPrograms;
	}

	/**
	 * @param dateFileApplication
	 *            the dateFileApplication to set
	 */
	public void setDateFileApplication(String dateFileApplication) {
		this.dateFileApplication = dateFileApplication;
	}

	/**
	 * @param dateFileGeneration
	 *            the dateFileGeneration to set
	 */
	public void setDateFileGeneration(String dateFileGeneration) {
		this.dateFileGeneration = dateFileGeneration;
	}

	/**
	 * @param programsApp
	 *            the programsApp to set
	 */
	public void setProgramsApp(Map<String, String> programsApp) {
		this.programsApp = programsApp;
	}

	/**
	 * @param skipOld
	 *            the skipOld to set
	 */
	public void setSkipOld(boolean skipOld) {
		this.skipOld = skipOld;
	}

	/**
	 * @param typePatch
	 *            the typePatch to set
	 */
	public void setTypePatch(int typePatch) {
		this.typePatch = typePatch;
	}

}

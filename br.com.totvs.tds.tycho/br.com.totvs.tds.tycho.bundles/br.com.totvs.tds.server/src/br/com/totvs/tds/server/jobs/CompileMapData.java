package br.com.totvs.tds.server.jobs;

import java.util.ArrayList;
import java.util.List;

public class CompileMapData {
	private List<String> files = new ArrayList<String>();
	private List<String> includePaths = new ArrayList<String>();
	/**
	 * @return the includePaths
	 */
	public List<String> getIncludePaths() {
		return includePaths;
	}
	/**
	 * @param includePaths the includePaths to set
	 */
	public void setIncludePaths(List<String> includePaths) {
		this.includePaths = includePaths;
	}
	/**
	 * @return the files
	 */
	public List<String> getFiles() {
		return files;
	}
	/**
	 * @param files the files to set
	 */
	public void setFiles(List<String> files) {
		this.files = files;
	}
}

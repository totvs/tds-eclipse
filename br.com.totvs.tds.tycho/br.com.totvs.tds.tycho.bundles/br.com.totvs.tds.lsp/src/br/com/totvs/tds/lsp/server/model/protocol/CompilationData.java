package br.com.totvs.tds.lsp.server.model.protocol;

public class CompilationData {

	CompilationInfo compilationInfo;

	/**
	 * @param compilationInfo
	 */
	public CompilationData(CompilationInfo compilationInfo) {
		this.compilationInfo = compilationInfo;
	}

	/**
	 * @return the compilationInfo
	 */
	public CompilationInfo getCompilationInfo() {
		return compilationInfo;
	}

	/**
	 * @param compilationInfo the compilationInfo to set
	 */
	public void setCompilationInfo(CompilationInfo compilationInfo) {
		this.compilationInfo = compilationInfo;
	}
}
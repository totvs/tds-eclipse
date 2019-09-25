package br.com.totvs.tds.lsp.server.model.protocol;

public class CompileOptions {

	private boolean recompile;
	private boolean debugAphInfo;
	private boolean gradualSending;
	private boolean generatePpoFile;
	private boolean showPreCompiler;
	private boolean priorVelocity;

	/**
	 * @param recompile
	 * @param debugAphInfo
	 * @param gradualSending
	 * @param generatePpoFile
	 * @param showPreCompiler
	 * @param priorVelocity
	 */

	public CompileOptions() {
		this(false, true, false, false, false, true);
	}

	public CompileOptions(boolean recompile, boolean debugAphInfo, boolean gradualSending, boolean generatePpoFile,
			boolean showPreCompiler, boolean priorVelocity) {
		this.recompile = recompile;
		this.debugAphInfo = debugAphInfo;
		this.gradualSending = gradualSending;
		this.generatePpoFile = generatePpoFile;
		this.showPreCompiler = showPreCompiler;
		this.priorVelocity = priorVelocity;
	}

	/**
	 * @return the recompile
	 */
	public boolean isRecompile() {
		return recompile;
	}

	/**
	 * @param recompile the recompile to set
	 */
	public void setRecompile(boolean recompile) {
		this.recompile = recompile;
	}

	/**
	 * @return the debugAphInfo
	 */
	public boolean isDebugAphInfo() {
		return debugAphInfo;
	}

	/**
	 * @param debugAphInfo the debugAphInfo to set
	 */
	public void setDebugAphInfo(boolean debugAphInfo) {
		this.debugAphInfo = debugAphInfo;
	}

	/**
	 * @return the gradualSending
	 */
	public boolean isGradualSending() {
		return gradualSending;
	}

	/**
	 * @param gradualSending the gradualSending to set
	 */
	public void setGradualSending(boolean gradualSending) {
		this.gradualSending = gradualSending;
	}

	/**
	 * @return the generatePpoFile
	 */
	public boolean isGeneratePpoFile() {
		return generatePpoFile;
	}

	/**
	 * @param generatePpoFile the generatePpoFile to set
	 */
	public void setGeneratePpoFile(boolean generatePpoFile) {
		this.generatePpoFile = generatePpoFile;
	}

	/**
	 * @return the showPreCompiler
	 */
	public boolean isShowPreCompiler() {
		return showPreCompiler;
	}

	/**
	 * @param showPreCompiler the showPreCompiler to set
	 */
	public void setShowPreCompiler(boolean showPreCompiler) {
		this.showPreCompiler = showPreCompiler;
	}

	/**
	 * @return the priorVelocity
	 */
	public boolean isPriorVelocity() {
		return priorVelocity;
	}

	/**
	 * @param priorVelocity the priorVelocity to set
	 */
	public void setPriorVelocity(boolean priorVelocity) {
		this.priorVelocity = priorVelocity;
	}

}

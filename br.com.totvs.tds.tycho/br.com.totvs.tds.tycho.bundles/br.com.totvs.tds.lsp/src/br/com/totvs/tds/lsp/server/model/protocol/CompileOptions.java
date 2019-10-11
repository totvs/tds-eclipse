package br.com.totvs.tds.lsp.server.model.protocol;

import java.util.Collections;
import java.util.List;

public class CompileOptions {

	private boolean recompile;
	private boolean debugAphInfo;
	private boolean gradualSending;
	private boolean generatePpoFile;
	private boolean showPreCompiler;
	private boolean priorVelocity;
	private List<String> resources;

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

	public CompileOptions(final boolean recompile, final boolean debugAphInfo, final boolean gradualSending,
			final boolean generatePpoFile, final boolean showPreCompiler, final boolean priorVelocity) {
		this.recompile = recompile;
		this.debugAphInfo = debugAphInfo;
		this.gradualSending = gradualSending;
		this.generatePpoFile = generatePpoFile;
		this.showPreCompiler = showPreCompiler;
		this.priorVelocity = priorVelocity;
		this.resources = Collections.emptyList();
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
	public void setRecompile(final boolean recompile) {
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
	public void setDebugAphInfo(final boolean debugAphInfo) {
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
	public void setGradualSending(final boolean gradualSending) {
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
	public void setGeneratePpoFile(final boolean generatePpoFile) {
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
	public void setShowPreCompiler(final boolean showPreCompiler) {
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
	public void setPriorVelocity(final boolean priorVelocity) {
		this.priorVelocity = priorVelocity;
	}

	public List<String> getResources() {
		return resources;
	}

	public void setResources(final List<String> resources) {
		this.resources = resources;
	}

}

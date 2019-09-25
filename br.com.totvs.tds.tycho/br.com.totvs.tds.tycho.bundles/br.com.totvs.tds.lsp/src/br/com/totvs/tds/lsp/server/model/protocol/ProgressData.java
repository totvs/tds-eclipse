package br.com.totvs.tds.lsp.server.model.protocol;

public class ProgressData {
	private int activeThreads;
	private int doIdMapCount;
	private int indexRequestCount;
	private int loadPreviousIndexCount;
	private int onIdMappedCount;
	private int onIndexedCount;

	public ProgressData(int indexRequestCount, int doIdMapCount, int loadPreviousIndexCount, int onIdMappedCount,
			int onIndexedCount, int activeThreads) {
		this.indexRequestCount = indexRequestCount;
		this.doIdMapCount = doIdMapCount;
		this.loadPreviousIndexCount = loadPreviousIndexCount;
		this.onIdMappedCount = onIdMappedCount;
		this.onIndexedCount = onIndexedCount;
		this.activeThreads = activeThreads;
	}

	public int getActiveThreads() {
		return activeThreads;
	}

	public int getDoIdMapCount() {
		return doIdMapCount;
	}

	public int getIndexRequestCount() {
		return indexRequestCount;
	}

	public int getLoadPreviousIndexCount() {
		return loadPreviousIndexCount;
	}

	public int getOnIdMappedCount() {
		return onIdMappedCount;
	}

	public int getOnIndexedCount() {
		return onIndexedCount;
	}
}
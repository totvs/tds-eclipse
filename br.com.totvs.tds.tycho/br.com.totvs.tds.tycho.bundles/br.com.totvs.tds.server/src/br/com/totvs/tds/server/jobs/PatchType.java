package br.com.totvs.tds.server.jobs;

/**
 * Enum para representar os tipos de patch.
 */
public enum PatchType {
	PATCH_UPD(1), PATCH_PAK(2), PATCH_PTM(3);

	private int patchType = 0;

	private PatchType(final int code) {
		patchType = code;
	}

	public int getPatchType() {
		return patchType;
	}

	public String getExtension() {
		String name = name();
		int pos = name.indexOf("_") + 1; //$NON-NLS-1$

		return name.substring(pos, pos + 3).toLowerCase();
	}

};

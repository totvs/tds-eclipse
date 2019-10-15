
import java.util.List;

import br.com.totvs.tds.server.interfaces.IValidationPatchReturn;
import br.com.totvs.tds.server.jobs.ApplyPatchMode;
import br.com.totvs.tds.server.jobs.ServerReturn;

/**
 * ValidationPatchReturn.
 *
 * @author leo.watanabe
 *
 */
public class ValidationPatchReturn extends ServerReturn implements IValidationPatchReturn {

	private List<String[]> oldPrograms;
	private boolean isSigned = false;
	private ApplyPatchMode applyPatchMode;

	/**
	 * Construtor.
	 *
	 * @param retSuccess
	 * @param retMessage
	 */
	public ValidationPatchReturn(final boolean retSuccess, final String retMessage) {
		super(retSuccess, retMessage);
	}

	/**
	 * Construtor.
	 *
	 * @param retSuccess
	 */
	public ValidationPatchReturn(final boolean retSuccess) {
		super(retSuccess, null);
	}

	public ValidationPatchReturn() {
		super(false, null);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.core.messageservice.statusreturn.IValidationPatchReturn#
	 * getOldPrograms()
	 */
	@Override
	public final List<String[]> getOldPrograms() {
		return this.oldPrograms;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.core.messageservice.statusreturn.IValidationPatchReturn#
	 * setOldPrograms(java.util.List)
	 */
	@Override
	public final void setOldPrograms(final List<String[]> oldProgramsReturn) {
		this.oldPrograms = oldProgramsReturn;
	}

	public boolean isSigned() {
		return isSigned;
	}

	public void setSigned(final boolean isSigned) {
		this.isSigned = isSigned;
	}

	public void setApplyPatchMode(final ApplyPatchMode applyPatchMode) {
		this.applyPatchMode = applyPatchMode;
	}

	public ApplyPatchMode getApplyPatchMode() {
		return this.applyPatchMode;
	}

}

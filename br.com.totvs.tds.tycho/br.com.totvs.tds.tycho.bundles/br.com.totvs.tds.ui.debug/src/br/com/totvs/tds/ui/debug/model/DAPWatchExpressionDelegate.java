/**
 *
 */
package br.com.totvs.tds.ui.debug.model;

import java.util.concurrent.ExecutionException;

import org.eclipse.core.runtime.Adapters;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.debug.core.DebugException;
import org.eclipse.debug.core.model.IDebugElement;
import org.eclipse.debug.core.model.IValue;
import org.eclipse.debug.core.model.IWatchExpressionDelegate;
import org.eclipse.debug.core.model.IWatchExpressionListener;
import org.eclipse.debug.core.model.IWatchExpressionResult;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.lsp4e.debug.DSPPlugin;
import org.eclipse.lsp4e.debug.debugmodel.DSPDebugTarget;
import org.eclipse.lsp4e.debug.debugmodel.DSPStackFrame;
import org.eclipse.lsp4e.debug.debugmodel.DSPValue;
import org.eclipse.lsp4e.debug.debugmodel.DSPVariable;
import org.eclipse.lsp4j.debug.EvaluateArguments;
import org.eclipse.lsp4j.debug.EvaluateArgumentsContext;
import org.eclipse.lsp4j.debug.EvaluateResponse;

/**
 * @author acandido
 *
 */
@SuppressWarnings("restriction")
public class DAPWatchExpressionDelegate implements IWatchExpressionDelegate {

	@Override
	public void evaluateExpression(final String expression, final IDebugElement context,
			final IWatchExpressionListener listener) {

		final DSPStackFrame frame = this.getFrame();
		if (frame == null) {
			return;
		}

		final EvaluateArguments args = new EvaluateArguments();
		args.setContext(EvaluateArgumentsContext.WATCH);
		args.setFrameId(frame.getFrameId());
		args.setExpression(expression);
		try {
			final EvaluateResponse res = frame.getDebugProtocolServer().evaluate(args).get();
			final DSPDebugTarget debugTarget = frame.getDebugTarget();
			final Long parentVariablesReference = res.getVariablesReference();
			final Long childrenVariablesReference = 0L;
			final String name = args.getExpression();
			final DSPVariable variable = new DSPVariable(debugTarget, parentVariablesReference, name, "**",
					childrenVariablesReference);
			final DSPValue value = new DSPValue(variable, res.getVariablesReference(), res.getResult());

			listener.watchEvaluationFinished(new IWatchExpressionResult() {
				@Override
				public IValue getValue() {

					return value;
				}

				@Override
				public boolean hasErrors() {
					return false;
				}

				@Override
				public String[] getErrorMessages() {
					return new String[0];
				}

				@Override
				public String getExpressionText() {
					return expression;
				}

				@Override
				public DebugException getException() {
					return null;
				}
			});

		} catch (InterruptedException | ExecutionException e) {
			DSPPlugin.logError(e);
		}

	}

	private DSPStackFrame getFrame() {
		final IAdaptable adaptable = DebugUITools.getDebugContext();
		if (adaptable != null) {
			return Adapters.adapt(adaptable, DSPStackFrame.class);
		}
		return null;
	}

}

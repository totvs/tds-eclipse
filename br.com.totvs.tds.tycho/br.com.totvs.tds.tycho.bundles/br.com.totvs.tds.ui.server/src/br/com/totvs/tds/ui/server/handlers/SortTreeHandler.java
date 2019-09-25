package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.ui.handlers.HandlerUtil;

public class SortTreeHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		Command command = event.getCommand();
		boolean oldValue = HandlerUtil.toggleCommandState(command);

		return !oldValue; // inverte para retornar o valor corrente
	}

}

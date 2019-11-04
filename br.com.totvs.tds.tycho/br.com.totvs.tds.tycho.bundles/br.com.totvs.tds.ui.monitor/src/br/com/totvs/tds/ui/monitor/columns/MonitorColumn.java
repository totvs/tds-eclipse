package br.com.totvs.tds.ui.monitor.columns;

import java.util.ArrayList;
import java.util.List;

/**
 * Colunas que estão sendo monitoradas pelo servidor.
 *
 * @author eriky.kashivagui
 *
 */
public final class MonitorColumn {

	private static List<MonitorColumn> columnsToMonitor;

	private static final int SERVER_NAME = 0;
	private static final int ENVIRONMENT = 1;
	private static final int COMPUTER_NAME = 2;
	private static final int THREAD_ID = 3;
	private static final int USER_SERVER = 4;
	private static final int PROGRAM = 5;
	private static final int CONECTION = 6;
	private static final int TIME_ELAPSED = 7;
	private static final int INSTRUCTION = 8;
	private static final int INSTRUCTION_PER_SECONDS = 9;
	private static final int OBSERVATION = 10;
	private static final int MEMORY = 11;
	private static final int SID = 12;
	private static final int RPO = 13;
	private static final int INACTIVITY = 14;
	private static final int TYPE_CONNECTION = 15;

	public MonitorColumn(final String name, final boolean fixed, final int order) {
		this.name = name;
		this.fixed = fixed;
		this.order = order;
		this.size = 150;
	}

	public MonitorColumn() {
	}

	/**
	 * Nome da coluna.
	 */
	private String name;

	/**
	 * Informa se a posição da coluna é fixa.
	 */
	private boolean fixed;

	/**
	 * Informa o tamanho da coluna.
	 */
	private int size;

	/**
	 * Informa o posicionamento da coluna.
	 */
	private int order;

	/**
	 * Retorna o nome da coluna.
	 *
	 * @return Retorna o nome da coluna.
	 */
	public String getName() {
		return name;
	}

	/**
	 * Insere o nome da coluna.
	 *
	 * @param name - Nome da coluna.
	 */
	public void setName(final String name) {
		this.name = name;
	}

	/**
	 * Retorna se a coluna é fixa.
	 *
	 * @return Retorna se a coluna é fixa.
	 */
	public boolean isFixed() {
		return fixed;
	}

	/**
	 * Insere se a coluna é fixa.
	 *
	 * @param fixed - Informa se a coluna é fixa.
	 */
	public void setFixed(final boolean fixed) {
		this.fixed = fixed;
	}

	/**
	 * Retorna o tamanho da coluna.
	 *
	 * @return Retorna o tamanho da coluna.
	 */
	public int getSize() {
		return size;
	}

	/**
	 * Insere o tamanho da coluna.
	 *
	 * @param size - Tamanho da coluna.
	 */
	public void setSize(final int size) {
		this.size = size;
	}

	/**
	 * Retorna a posição da coluna.
	 *
	 * @return Retorna a posição da coluna.
	 */
	public int getOrder() {
		return order;
	}

	/**
	 * Insere a posição da coluna.
	 *
	 * @param order - Posição da coluna.
	 */
	public void setOrder(final int order) {
		this.order = order;
	}

	public static List<MonitorColumn> getColumnsToMonitor() {
		if (columnsToMonitor == null) {
			columnsToMonitor = new ArrayList<MonitorColumn>();

			columnsToMonitor.add(new MonitorColumn("Servidor", false, SERVER_NAME));
			columnsToMonitor.add(new MonitorColumn("Ambiente", false, ENVIRONMENT));
			columnsToMonitor.add(new MonitorColumn("Estação", false, COMPUTER_NAME));
			columnsToMonitor.add(new MonitorColumn("THREAD_ID", false, THREAD_ID));
			columnsToMonitor.add(new MonitorColumn("Usuário", false, USER_SERVER));
			columnsToMonitor.add(new MonitorColumn("Programa", false, PROGRAM));
			columnsToMonitor.add(new MonitorColumn("Conexão", false, CONECTION));
			columnsToMonitor.add(new MonitorColumn("Tempo", false, TIME_ELAPSED));
			columnsToMonitor.add(new MonitorColumn("Instruções", false, INSTRUCTION));
			columnsToMonitor.add(new MonitorColumn("Instruções/seg", false, INSTRUCTION_PER_SECONDS));
			columnsToMonitor.add(new MonitorColumn("Observação", false, OBSERVATION));
			columnsToMonitor.add(new MonitorColumn("Memória", false, MEMORY));
			columnsToMonitor.add(new MonitorColumn("SID", false, SID));
			columnsToMonitor.add(new MonitorColumn("RPO", false, RPO));
			columnsToMonitor.add(new MonitorColumn("Inatividade", false, INACTIVITY));
			columnsToMonitor.add(new MonitorColumn("Conexão", false, TYPE_CONNECTION));
		}
		return columnsToMonitor;
	}
}

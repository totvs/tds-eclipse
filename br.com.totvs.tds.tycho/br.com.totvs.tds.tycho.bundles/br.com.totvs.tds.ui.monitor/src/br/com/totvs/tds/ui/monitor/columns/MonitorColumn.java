package br.com.totvs.tds.ui.monitor.columns;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;

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

	private int alignment;

	public MonitorColumn(final String name, final int order) {
		this(name, order, SWT.LEFT, false);
	}

	public MonitorColumn() {
	}

	public MonitorColumn(final String name, final int order, final int alignment) {
		this(name, order, alignment, false);
	}

	public MonitorColumn(final String name, final int order, final int alignment, final boolean fixed) {
		this.name = name;
		this.alignment = alignment;
		this.fixed = fixed;
		this.order = order;
		this.size = 75;
	}

	public MonitorColumn(final String name, final int order, final boolean fixed) {
		this(name, order, SWT.LEFT, fixed);
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

			columnsToMonitor.add(new MonitorColumn("Servidor", SERVER_NAME, true));
			columnsToMonitor.add(new MonitorColumn("Ambiente", ENVIRONMENT, true));
			columnsToMonitor.add(new MonitorColumn("Estação", COMPUTER_NAME));
			columnsToMonitor.add(new MonitorColumn("ID", THREAD_ID, SWT.RIGHT));
			columnsToMonitor.add(new MonitorColumn("Usuário", USER_SERVER));
			columnsToMonitor.add(new MonitorColumn("Programa", PROGRAM));
			columnsToMonitor.add(new MonitorColumn("Conexão", CONECTION));
			columnsToMonitor.add(new MonitorColumn("Tempo", TIME_ELAPSED, SWT.RIGHT));
			columnsToMonitor.add(new MonitorColumn("Instruções", INSTRUCTION, SWT.RIGHT));
			columnsToMonitor.add(new MonitorColumn("Instruções/seg", INSTRUCTION_PER_SECONDS, SWT.RIGHT));
			columnsToMonitor.add(new MonitorColumn("Observação", OBSERVATION));
			columnsToMonitor.add(new MonitorColumn("Memória", MEMORY, SWT.RIGHT));
			columnsToMonitor.add(new MonitorColumn("SID", SID));
			columnsToMonitor.add(new MonitorColumn("RPO", RPO));
			columnsToMonitor.add(new MonitorColumn("Inatividade", INACTIVITY, SWT.RIGHT));
			columnsToMonitor.add(new MonitorColumn("Conexão", TYPE_CONNECTION));

			columnsToMonitor.get(SERVER_NAME).setSize(150);
			columnsToMonitor.get(ENVIRONMENT).setSize(150);
			columnsToMonitor.get(COMPUTER_NAME).setSize(150);
			columnsToMonitor.get(OBSERVATION).setSize(250);

		}
		return columnsToMonitor;
	}

	/**
	 * @return the alignment
	 */
	public int getAlignment() {
		return alignment;
	}

	/**
	 * @param alignment the align to set
	 */
	public void setAlignment(final int alignment) {
		this.alignment = alignment;
	}
}

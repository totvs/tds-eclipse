package br.com.totvs.tds.server.persistence;

import java.util.Iterator;
import java.util.Map;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class Moment {

	private static Moment instance = null;

	//	private static String TAG_AMBIENTE_ACTIVE_USER = "AMBIENTE_ATIVADO_USER"; //$NON-NLS-1$
	private static String TAG_ACTIVE_COMPANY = "ACTIVE_COMPANY"; //$NON-NLS-1$

	//	private static String TAG_SERVER_ACTIVE_USER = "SERVIDOR_ATIVADO_USER"; //$NON-NLS-1$

	//	private static String TAG_LAST_PROGRAM_NAME = "LAST_PROGRAM_NAME"; //$NON-NLS-1$
	private static String TAG_LAST_PARAMETER = "TAG_LAST_PARAMETER"; //$NON-NLS-1$

	//	private static String TAG_FONTE_EXPLORE = "FONTES_EXPLORE"; //$NON-NLS-1$
	private static String TAG_LAST_PROGRAM = "LAST_PROGRAM"; //$NON-NLS-1$
	private static String TAG_LISTA_FONTE = "FONTES_ATIVO"; //$NON-NLS-1$
	private static String TAG_LISTA_PARAMETER = "PARAMETER_ATIVO"; //$NON-NLS-1$
	public static Moment getInstance() {
		if (instance == null) {
			instance = new Moment();
		}
		return instance;
	}

	private ConcurrentMap<String, MomentBean> concurrentMap = new ConcurrentHashMap<String, MomentBean>();
	//	private static String TAG_SERVER_ACTIVE_TO_START = "SERVER_ACTIVE_TO_START"; //$NON-NLS-1$

	private Moment() {
		this.open();
	}

	// ..............................................................................
	// TRATAMENTO PARA ORGANIZAção (EMPRESA/FILAIL) ATIVO
	public void addActiveOrganization(String codeCompany, String nameCompany, String codeSubsidiary,
			String nameSubsidiary) {
		addMomento(Moment.TAG_ACTIVE_COMPANY, "COMPANY_CODE", codeCompany); //$NON-NLS-1$
		addMomento(Moment.TAG_ACTIVE_COMPANY, "COMPANY_NAME", nameCompany); //$NON-NLS-1$
		addMomento(Moment.TAG_ACTIVE_COMPANY, "SUBSIDIARY_CODE", codeSubsidiary); //$NON-NLS-1$
		addMomento(Moment.TAG_ACTIVE_COMPANY, "SUBSIDIARY_NAME", nameSubsidiary); //$NON-NLS-1$
	}

	public void addListaArquivo(String nome) {
		addMomento(TAG_LISTA_FONTE, nome, nome);
	}

	public void addListaParameter(String nome) {
		addMomento(Moment.TAG_LISTA_PARAMETER, nome, nome);
	}

	public String getLastParameter() {
		String valor = ""; //$NON-NLS-1$
		MomentBean momento = this.getTOTVSMomento(Moment.TAG_LAST_PROGRAM);
		if (momento != null) {
			valor = momento.getValString(TAG_LAST_PARAMETER); //$NON-NLS-1$ 
		}
		return valor;
	}

	public String getLastProgram() {
		String valor = ""; //$NON-NLS-1$
		MomentBean momento = this.getTOTVSMomento(Moment.TAG_LAST_PROGRAM);
		if (momento != null) {
			if (momento.getValString(TAG_LAST_PROGRAM) != null)
				valor = momento.getValString(TAG_LAST_PROGRAM); //$NON-NLS-1$ 
		}
		return valor;
	}

	public Vector<String> getNomeArquivo() {
		return getMomentoValues(Moment.TAG_LISTA_FONTE);
	}

	public Vector<String> getParameterFonte() {
		return getMomentoValues(Moment.TAG_LISTA_PARAMETER);
	}

	public void limparListaArquivo() {
		limparMomento(Moment.TAG_LISTA_FONTE);
	}

	public void limparListaParameter() {
		limparMomento(Moment.TAG_LISTA_PARAMETER);
	}

	public void salvar() {
		try {
			MomentPersistence.getInstancia().salvar(this.concurrentMap);
		} catch (Exception  e) {
			e.printStackTrace();
		}
	}

	public void setLastParameter(String param) {
		addMomento(TAG_LAST_PROGRAM, TAG_LAST_PARAMETER, param);
	}

	public void setLastProgram(String programName) {
		addMomento(Moment.TAG_LAST_PROGRAM, Moment.TAG_LAST_PROGRAM, programName);
	}

	private void addTOTVSMomento(MomentBean t) {
		if (this.concurrentMap.get(t.getId()) != null) {
			this.concurrentMap.remove(t.getId());
		}
		this.concurrentMap.put(t.getId(), t);
	}

	private Vector<String> getMomentoValues(String tagMomento) {
		Vector<String> vector = new Vector<String>();
		MomentBean momento = this.getTOTVSMomento(tagMomento);
		if (momento != null) {
			Map<String, Object> map = momento.getVal();
			for (Iterator<String> iterator = map.keySet().iterator(); iterator.hasNext();) {
				vector.add(iterator.next());
			}
		}
		return vector;
	}

	private MomentBean getTOTVSMomento(String tag) {
		return this.concurrentMap.get(tag);
	}

	private void open() {
		try {
			this.concurrentMap = MomentPersistence.getInstancia().open();
		} catch (Exception  e) {
			e.printStackTrace();
		}
	}

	protected void addMomento(String momentoTag, String keyObject, Object value) {
		MomentBean momento = this.getTOTVSMomento(momentoTag);
		if (momento == null) {
			momento = new MomentBean(momentoTag);
		}
		Object oldValue = momento.getValObject(keyObject);
		if (value == null) {
			momento.remover(keyObject);
		} else if (!value.equals(oldValue)) {
			momento.addVal(keyObject, value);
		}
		this.addTOTVSMomento(momento);
		salvar();
	}

	protected void limparMomento(String tagMomento) {
		MomentBean momento = new MomentBean(tagMomento);
		this.addTOTVSMomento(momento);
	}
}

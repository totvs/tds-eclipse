package br.com.totvs.tds.server.persistence;

import java.io.Serializable;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;
import java.util.concurrent.ConcurrentHashMap;

//XXX: Todo esse processo de persistencia tem que ser revisto. 
//Deixado aqui apenas para funcionamento da migração do Debug, mas deve ser removido desse ponto
//ou mesmo excluido para utilização de outro processo.

public class MomentBean implements Serializable {

	private static final long serialVersionUID = 2737280808397051318L;

	private String id = null;
	private String nome = null;
	private Map<String, Object> val = new ConcurrentHashMap<String, Object>();

	public MomentBean() {

	}

	public MomentBean(String key) {
		this.id = key;
	}

	public MomentBean(String id, String nome) {
		this.id = id;
		this.nome = nome;
	}

	public void addVal(String key, Object val) {
		if (val == null && key != null) {
			// tentativa de adicionar um val null para uma key existente
			this.val.remove(key);
		} else {
			this.val.put(key, val);
		}
	}

	public void addVal(String key, String val) {
		this.limparAutomaticamente();

		if (this.val.get(key) != null) {
			this.val.remove(key);
		}

		this.val.put(key, val);
	}

	public String getId() {
		return id;
	}

	public String getNome() {
		return nome;
	}

	@SuppressWarnings("rawtypes")
	public String[] getNomeKey() {
		int cont = 0;
		String[] s = new String[this.val.size()];
		for (Iterator iterator = this.val.keySet().iterator(); iterator.hasNext();) {
			String type = (String) iterator.next();
			s[cont++] = type;
		}
		return s;
	}

	public Map<String, Object> getVal() {
		return val;
	}

	public Object getValObject(String key) {
		return val.get(key);
	}

	public String getValString(String key) {
		if (val.get(key) == null)
			return null;
		return val.get(key).toString();
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void limparAutomaticamente() {
		int cont = 0;
		if (this.val.size() > 20) {
			Vector v = new Vector();
			for (Iterator iterator = this.val.keySet().iterator(); iterator.hasNext();) {
				String type = (String) iterator.next();
				cont++;
				if (cont > 20) {
					v.add(type);
				}
			}

			for (int i = 0; i < v.size(); i++) {
				String key = v.get(i).toString();
				this.remover(key);
			}

		}
	}

	public void limparLista(int valor) {
		if (valor < this.val.size()) {
			this.val.clear();
		}
	}

	public void remover(String key) {
		if (key != null && this.val.get(key) != null) {
			this.val.remove(key);
		}
	}

	public void setId(String id) {
		this.id = id;
	}

	public void setNome(String nome) {
		this.nome = nome;
	}

}

package br.com.totvs.tds.server.persistence;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.eclipse.core.runtime.Platform;
import org.eclipse.osgi.service.datalocation.Location;

import br.com.totvs.tds.server.ServerActivator;

public class MomentPersistence {
	private static MomentPersistence instancia = null;
	public static MomentPersistence getInstancia() {
		if (instancia == null) {
			instancia = new MomentPersistence();
		}
		return instancia;
	}
	private String nomeArquivo = ".state"; //$NON-NLS-1$
	private Location rootConfig = Platform.getConfigurationLocation();

	//private static String TAG_LAST_PROGRAM = "LAST_PROGRAM"; //$NON-NLS-1$

	private String url = null;

	private MomentPersistence() {
		if (rootConfig == null) {
			rootConfig = Platform.getConfigurationLocation();
		}
		if (ServerActivator.getDefault() != null) {
			//@@acandido@@ para que quardar a URL?
			String nUrl = ""; //ServerActivator.getDefault().getURLWorkSpace(); // rootConfig.getURL().getPath();
			this.url = nUrl + "" + nomeArquivo; //$NON-NLS-1$
		}
	}

	// RECUPERA OS DADOS DE COVAREGE
	@SuppressWarnings("unchecked")
	public ConcurrentMap<String, MomentBean> open() throws FileNotFoundException, IOException {
		ConcurrentHashMap<String, MomentBean> m = null;

		if (this.url != null) {
			File file = new File(this.url);
			ObjectInputStream in = null;
			try {
				in = new ObjectInputStream(new FileInputStream(file));
				m = (ConcurrentHashMap<String, MomentBean>) in.readObject();
			} catch (Exception  e) {
				System.out.println("ARQUIVO DE PERSISTENCIA NAO EXISTE"); //$NON-NLS-1$
			} finally {
				if (in != null) {
					in.close();
				}
				if (m == null) {
					System.out.println("CRIANDO NOVA BASE DE DADOS"); //$NON-NLS-1$
					m = new ConcurrentHashMap<String, MomentBean>();
					this.salvar(m);
				}
			}
		}
		return m;
	}

	public void remover(String nomekey) throws FileNotFoundException, IOException {
		ConcurrentMap<String, MomentBean> m = this.open();
		m.remove(nomekey.toUpperCase());
		this.salvar(m);
	}

	public void removerALL() throws FileNotFoundException, IOException {
		this.salvar(new ConcurrentHashMap<String, MomentBean>());
	}

	// PERSITE OS DADOS DE COVAREGE
	public void salvar(ConcurrentMap<String, MomentBean> object) throws FileNotFoundException, IOException {
		if (this.url != null) {
			ObjectOutput out = new ObjectOutputStream(new FileOutputStream(this.url));
			out.writeObject(object);
			out.close();
		}
	}

}

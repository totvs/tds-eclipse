package br.com.totvs.tds.ui.debug.helper;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;

public class TdsSourceContainer extends ProjectSourceContainer {

	public TdsSourceContainer(IProject project, boolean referenced) {
		super(project, referenced);
	}

	
	@Override
	public Object[] findSourceElements(String name) throws CoreException {
		// TODO Auto-generated method stub
		return super.findSourceElements(name);
	}

}

package br.com.totvs.tds.ui.debug.helper;

import java.util.ArrayList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.sourcelookup.ISourceContainer;
import org.eclipse.debug.core.sourcelookup.ISourcePathComputerDelegate;
import org.eclipse.debug.core.sourcelookup.containers.ProjectSourceContainer;

import br.com.totvs.tds.ui.sdk.builder.TotvsNature;

/**
 * 
 * @author acandido
 *
 */
public class SourcePathComputerDelegate implements ISourcePathComputerDelegate {

	public static final String ID = "br.com.totvs.tds.ui.debug.sourcePathComputer";

	private static final ISourceContainer[] EMPTY_ARRAY = new ISourceContainer[0];;

	private ISourceContainer[] sourceContainers = EMPTY_ARRAY;

	@Override
	public ISourceContainer[] computeSourceContainers(ILaunchConfiguration configuration, IProgressMonitor monitor)
			throws CoreException {

		if (sourceContainers.equals(EMPTY_ARRAY)) {
			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
			ArrayList<ISourceContainer> sourceContainerList = new ArrayList<ISourceContainer>();

			for (int i = 0; i < projects.length; i++) {
				if (projects[i].isAccessible()) {
					if (projects[i].hasNature(TotvsNature.NATURE_ID)) {
						ProjectSourceContainer psc = new ProjectSourceContainer(projects[i], true);
						sourceContainerList.add(psc);
					}
				}
			}
			sourceContainers = sourceContainerList.toArray(new ISourceContainer[sourceContainerList.size()]);
		}

		return sourceContainers;
	}

}
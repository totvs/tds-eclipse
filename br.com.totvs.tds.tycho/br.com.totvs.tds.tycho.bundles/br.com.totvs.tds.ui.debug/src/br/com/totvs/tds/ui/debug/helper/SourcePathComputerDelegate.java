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
@SuppressWarnings("restriction")
public class SourcePathComputerDelegate implements ISourcePathComputerDelegate {

	public static final String ID = "br.com.totvs.tds.ui.debug.sourcePathComputer"; //$NON-NLS-1$

	private static final ISourceContainer[] EMPTY_ARRAY = new ISourceContainer[0];

	private ISourceContainer[] sourceContainers = EMPTY_ARRAY;

	@SuppressWarnings("restriction")
	@Override
	public ISourceContainer[] computeSourceContainers(final ILaunchConfiguration configuration,
			final IProgressMonitor monitor) throws CoreException {

		if (sourceContainers.equals(EMPTY_ARRAY)) {
			final IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
			final ArrayList<ISourceContainer> sourceContainerList = new ArrayList<ISourceContainer>();

			for (int i = 0; i < projects.length; i++) {
				if (projects[i].isAccessible()) {
					if (projects[i].hasNature(TotvsNature.NATURE_ID)) {
						final ProjectSourceContainer psc = new ProjectSourceContainer(projects[i], true);
						sourceContainerList.add(psc);
					}
				}
			}
			// sourceContainerList.add(new AbsolutePathSourceContainer());

			sourceContainers = sourceContainerList.toArray(new ISourceContainer[sourceContainerList.size()]);
		}

		return sourceContainers;
	}

}
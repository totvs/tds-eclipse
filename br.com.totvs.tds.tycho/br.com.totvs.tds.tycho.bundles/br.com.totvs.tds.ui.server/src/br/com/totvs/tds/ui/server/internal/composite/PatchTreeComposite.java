package br.com.totvs.tds.ui.server.internal.composite;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.interfaces.IPatchRpoInfo;
import br.com.totvs.tds.server.interfaces.IRpoInfo;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.nl.Messages;

public class PatchTreeComposite extends Composite {

	/**
	 * Content provider da �rvore de patch.
	 *
	 * @author eriky.kashivagui
	 *
	 */
	private class PatchContentProvider implements ITreeContentProvider {
		@Override
		public void dispose() {
		}

		@SuppressWarnings("unchecked")
		@Override
		public Object[] getChildren(final Object parent) {
			if (parent instanceof List) {
				return ((List<IRpoInfo>) parent).toArray();
			} else if (parent instanceof IRpoInfo) {
				IRpoInfo rpoInfo = (IRpoInfo) parent;
				List<IPatchRpoInfo> rpoPatchs = rpoInfo.getRpoPatchs();

				Map<String, PathRpoContainer> temp = new HashMap<>();
				for (IPatchRpoInfo rpoPatch : rpoPatchs) {
					if (temp.containsKey(rpoPatch.getDateFileApplication())) {
						temp.get(rpoPatch.getDateFileApplication()).addPathByDate(rpoPatch);
					} else {
						PathRpoContainer pathContainer = new PathRpoContainer();
						SimpleDateFormat dateFormatter = new SimpleDateFormat("dd/MM/yyyy"); //$NON-NLS-1$
						Date date = null;
						try {
							date = dateFormatter.parse(rpoPatch.getDateFileApplication());
						} catch (ParseException e) {
							ServerUIActivator.logStatus(IStatus.ERROR, Messages.PatchTreeComposite_internal,
									e.getMessage(), e);
						}
						Locale localeBR = new Locale("pt", "BR"); //$NON-NLS-1$ //$NON-NLS-2$
						DateFormat formatter = new SimpleDateFormat("EEEE, dd MMMM yyyy", localeBR); //$NON-NLS-1$
						String formatedDate = formatter.format(date);
						pathContainer.setLabel(formatedDate);

						pathContainer.addPathByDate(rpoPatch);
						temp.put(rpoPatch.getDateFileApplication(), pathContainer);
					}
				}
				return temp.values().toArray();
			} else if (parent instanceof PathRpoContainer) {
				return ((PathRpoContainer) parent).getPathByDate().toArray();
			} else {
				return new Object[0];
			}

		}

		@Override
		public Object[] getElements(final Object parent) {
			return getChildren(parent);
		}

		@Override
		public Object getParent(final Object child) {
			if (child instanceof List) {
				return null;
			} else if (child instanceof IRpoInfo) {
				return rpoInfos;
			} else if (child instanceof IPatchRpoInfo) {
				if (rpoInfos.contains(child)) {
					return rpoInfos.get(rpoInfos.indexOf(child));
				}
				return null;
			}
			return null;

		}

		@Override
		public boolean hasChildren(final Object parent) {
			if (parent instanceof List || parent instanceof IRpoInfo || parent instanceof PathRpoContainer) {
				return true;
			}
			return false;
		}

		@Override
		public void inputChanged(final Viewer v, final Object oldInput, final Object newInput) {
		}
	}

	/**
	 * Label provider da �rvore de patch.
	 *
	 * @author eriky.kashivagui
	 *
	 */
	private class PatchLabelProvider extends LabelProvider {
		@Override
		public Image getImage(final Object obj) {
			Image igmNode = PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_SAVE_EDIT);
			if (obj instanceof IRpoInfo) {
				return ServerUIIcons.getServer().createImage();
			} else if (obj instanceof PathRpoContainer) {
				return ServerUIIcons.getDate().createImage();
			}
			return igmNode;
		}

		@Override
		public String getText(final Object obj) {
			if (obj instanceof IRpoInfo) {
				return ((IRpoInfo) obj).getEnvironment();
			} else if (obj instanceof IPatchRpoInfo) {
				return ((IPatchRpoInfo) obj).getDateFileApplication();
			} else if (obj instanceof PathRpoContainer) {
				return ((PathRpoContainer) obj).getLabel();
			} else {
				return obj.toString();
			}
		}
	}

	/**
	 * Container de IPatchRpoInfo's para etiquetação por data.
	 *
	 * @author eriky.kashivagui
	 *
	 */
	private class PathRpoContainer {

		/**
		 * Etiqueta de datas
		 */
		private String label;

		/**
		 * Lista de elementos com os caminhos de patch rpo.
		 */
		private final List<IPatchRpoInfo> patchByDate;

		/**
		 * Construtor.
		 */
		public PathRpoContainer() {
			patchByDate = new ArrayList<IPatchRpoInfo>();
		}

		/**
		 *
		 * @param patchByDate - Patch
		 */
		private void addPathByDate(IPatchRpoInfo patchByDate) {
			this.patchByDate.add(patchByDate);

		}

		/**
		 * @return the label
		 */
		public final String getLabel() {
			return label;
		}

		/**
		 * @return the pathByDate
		 */
		public final List<IPatchRpoInfo> getPathByDate() {
			return patchByDate;
		}

		/**
		 * @param label the label to set
		 */
		public final void setLabel(String label) {
			this.label = label;
		}
	}

	/**
	 * Lista de rpos que ser�o visualizados.
	 */
	private final List<IRpoInfo> rpoInfos;

	private TreeViewer treePatches;

	public PatchTreeComposite(Composite composite, List<IRpoInfo> rpoInfos, int style) {
		super(composite, style);
		if (rpoInfos == null) {
			rpoInfos = new ArrayList<IRpoInfo>();
		}
		this.rpoInfos = rpoInfos;
		initialize(style);
	}

	public void addTreeSelectionListener(final ISelectionChangedListener listener) {
		if (treePatches != null) {
			treePatches.addSelectionChangedListener(listener);
		}
	}

	public void clearTree() {
		rpoInfos.clear();
		treePatches.refresh();
	}

	private void initialize(final int style) {
		this.setLayout(new FillLayout(SWT.HORIZONTAL | SWT.VERTICAL));
		this.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		treePatches = new TreeViewer(this, style);
		treePatches.setContentProvider(new PatchContentProvider());
		ILabelDecorator decorator = PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator();
		DecoratingLabelProvider labelProvider = new DecoratingLabelProvider(new PatchLabelProvider(), decorator);
		treePatches.setLabelProvider(labelProvider);
		treePatches.setInput(rpoInfos);
		treePatches.refresh();
	}

	public void refreshTree(final IRpoInfo rpoInfo) {
		rpoInfos.clear();
		rpoInfos.add(rpoInfo);
		treePatches.refresh();
	}

}

package br.com.totvs.tds.server.rulers;

import java.util.UUID;

import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.MultiRule;

public class ServerRules {
	private static ServerRules serverRules = new ServerRules();

	public static final ISchedulingRule connectionRule = new ISchedulingRule() {

		@Override
		public boolean contains(final ISchedulingRule rule) {
			return rule == this;
		}

		@Override
		public boolean isConflicting(final ISchedulingRule rule) {
			return rule == this;
		}
	};

	class ServerRule implements ISchedulingRule {

		final private UUID id;

		public ServerRule(final UUID id) {
			this.id = id;
		}

		@Override
		public boolean contains(final ISchedulingRule rule) {
			return rule == this;
		}

		@Override
		public boolean isConflicting(final ISchedulingRule rule) {
			if (rule instanceof ServerRule) {
				final ServerRule serverRule = (ServerRule) rule;

				return this.id.equals(serverRule.id);
			}

			return false;
		}

	}

	public static ISchedulingRule compileRule(final UUID id) {
		final ISchedulingRule[] rules = new ISchedulingRule[2];
		rules[0] = connectionRule;
		rules[1] = serverRules.new ServerRule(id);

		return new MultiRule(rules);
	}

}

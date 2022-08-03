import traceback
from typing import List

import cloudpickle as pickle

from pyraphtory.steps import Iterate, Step, State, StepState, GlobalSelect
from pyraphtory.proxy import GenericScalaProxy
from pyraphtory.interop import register, logger, to_jvm


class ProgressTracker(GenericScalaProxy):
    _classname = "com.raphtory.api.querytracker.QueryProgressTracker"

    def inner_tracker(self):
        logger.trace("Progress tracker inner tracker returned")
        return to_jvm(self)


@register(name="Table")
class Table(GenericScalaProxy):
    pass


@register(name="TemporalGraph")
class TemporalGraph(GenericScalaProxy):

    def set_global_state(self, s: State):
        state_bytes = pickle.dumps(s)
        return self.python_set_global_state(state_bytes)

    def step(self, s: Step):
        logger.trace("step called")
        step_bytes = pickle.dumps(s)
        return self.python_step(step_bytes)

    def iterate(self, i: Iterate):
        logger.trace("iterate called")
        iterate_bytes = pickle.dumps(i)
        return self.python_iterate(iterate_bytes, i.iterations, i.execute_messaged_only)

    def select(self, columns: List[str]):
        logger.trace("select called")
        return self.python_select(columns)

    def select_state(self, columns: List[str]):
        return self.python_select_state(columns)

    def step_state(self, ssb: StepState):
        logger.trace("step_state called")
        step_state_bytes = pickle.dumps(ssb)
        return self.python_step_state(step_state_bytes)

    def global_select(self, gs: GlobalSelect):
        logger.trace("global_select called")
        global_select_bytes = pickle.dumps(gs)
        return self.python_global_select(global_select_bytes)

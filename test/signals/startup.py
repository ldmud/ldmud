import ldmud

def signal_hook():
    ldmud.efuns.shutdown(0)

def expect_signal(sig: str) -> None:
    ldmud.register_hook(getattr(ldmud, "ON_" + sig), signal_hook)

ldmud.register_efun("expect_signal", expect_signal)

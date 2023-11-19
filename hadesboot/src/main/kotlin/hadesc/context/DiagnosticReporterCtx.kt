package hadesc.context

import hadesc.diagnostics.DiagnosticReporter

interface DiagnosticReporterCtx {
    val diagnosticReporter: DiagnosticReporter
}
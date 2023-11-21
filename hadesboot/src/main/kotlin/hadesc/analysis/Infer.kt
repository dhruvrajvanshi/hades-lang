package hadesc.analysis

interface Infer {
    companion object {

        fun make(): Infer = InferImpl()
    }
}
private class InferImpl: Infer {

}
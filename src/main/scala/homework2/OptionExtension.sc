implicit class OptionExtension(option: Option[Int]) {
  def default: Int = option.getOrElse(0)
}

# Quant-system
大创项目《量化投资策略有效性智能评估系统开发》代码仓库<br>
demo：MACD指标策略<br>
Timing1.0：择时策略初稿<br>
Timing1.1：改进策略，以ppv为指标进行搜索取最优组合后测PBO<br>
Timing1.2：加入评估夏普率的函数，将后面搜索功能制成函数<br>
Timing1.3：实现CSCV，拼接成完整的代码
Timing1.4：最终版本

Indicators存放了约40个ta库中的指标，在此基础上编制简单策略，分别对每个策略选取一定超参空间计算PBO，验证过拟合概率。

package com.samsph.pjcm.query;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

/**
 * @author hujiahao
 */

@ApiModel(description = "投稿加入到某一期期刊请求实体")
@Data
public class PostJournalQuery {
    @ApiModelProperty("稿件id")
    @NotNull(message = "id不能为空")
    private Integer id;

    @ApiModelProperty("期刊id")
    @NotNull(message = "期刊id不能为空")
    private Integer jid;
}

package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.query.ReviewRecordQuery;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author hujiahao
 */
@Slf4j
@RestController
@RequestMapping("/api/v1/reviewRecords")
@Api(tags = "4. 审稿记录管理")
public class ReviewRecordController {
    @PostMapping()
    @ApiOperation(value = "审稿人提交审稿记录")
    public AjaxResponse review(@RequestBody ReviewRecordQuery reviewRecordQuery) {
        // 首轮审稿可以否决和转送

        // 再审稿只能通过或建议修改

        return AjaxResponse.success();
    }
}

package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.service.JournalService;
import com.samsph.pjcm.query.Add;
import com.samsph.pjcm.query.JournalQuery;
import com.samsph.pjcm.query.Update;
import io.swagger.annotations.*;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

/**
 * @author hujiahao
 */

@Slf4j
@RestController
@Validated
@RequestMapping("/api/v1/journals")
@Api(tags = "1. 期刊管理")
public class JournalController {
    @Resource(name = "journalServiceImpl")
    JournalService journalService;

    @PostMapping()
    @ApiOperation(value = "创建期刊")
    @ApiImplicitParam(name = "journalQuery", value = "必填：year、month、day；其余不填", dataType = "JournalQuery")
    public AjaxResponse saveJournal(@Validated({Add.class}) @RequestBody JournalQuery journalQuery) {
        // TODO: 检查用户权限
        log.info("saveJournal: {}", journalQuery);
        // TODO：创建者id写死为7
        return AjaxResponse.success(journalService.saveJournal(journalQuery,7));
    }

    @PutMapping()
    @ApiOperation(value = "更新期刊")
    @ApiImplicitParam(name = "journalQuery", value = "必填：id；选填：year、month、day；其余不填", dataType = "JournalQuery")
    public AjaxResponse updateJournal(@Validated({Update.class}) @RequestBody JournalQuery journalQuery) {
        // TODO: 检查用户权限
        journalService.updateJournal(journalQuery);
        return AjaxResponse.success();
    }


    @GetMapping()
    @ApiOperation(value = "获取期刊列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "page", value = "分页页号", required = true, dataType = "int"),
            @ApiImplicitParam(name = "size", value = "分页大小", required = true, dataType = "int"),
            @ApiImplicitParam(name = "ascend", value = "是否升序，默认为true", dataType = "boolean"),
            @ApiImplicitParam(name = "sortBy", value = "如何排序，默认为id")
    })
    public AjaxResponse getAll(@NotNull(message = "page不能为空") @RequestParam("page") Integer page,
                               @NotNull @RequestParam("size") Integer size,
                               @RequestParam(value = "sortBy", required = false) String sortBy,
                               @RequestParam(value = "ascend", required = false) Boolean ascend) {
        if (sortBy == null) {
            sortBy = "id";
        }
        if (ascend == null) {
            ascend = true;
        }
        return AjaxResponse.success(journalService.getAll(page, size, sortBy, ascend));
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "根据id获取期刊详细信息")
    public AjaxResponse getJournal(@NotNull @PathVariable Integer id) {
        return AjaxResponse.success(journalService.getJournal(id));
    }


    @GetMapping("/")
    @ApiOperation(value = "根据年月卷期获取期刊详细信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "year", required = true, value = "年号，2000到2050", dataType = "int"),
            @ApiImplicitParam(name = "month", required = true, value = "月号，1到12", dataType = "int"),
            @ApiImplicitParam(name = "volume", required = true, value = "卷号，大于0", dataType = "int"),
            @ApiImplicitParam(name = "number", required = true, value = "期号，大于0", dataType = "int")
    })
    public AjaxResponse getJournalByNumber(@NotNull @Max(value = 2050, message = "年号不得超过2050") @Min(value = 2000, message = "年号不得小于2000") @RequestParam("year") Integer year,
                                           @NotNull @Max(value = 12, message = "月号不得超过12") @Min(value = 1, message = "月号不得小于1") @RequestParam("month") Integer month,
                                           @NotNull @Min(value = 1, message = "卷号至少为1") @RequestParam("volume") Integer volume,
                                           @NotNull @Min(value = 1, message = "期号至少为1") @RequestParam("number") Integer number) {
        return AjaxResponse.success(journalService.getJournal(year, month, volume, number));
    }


    @ApiOperation(value = "根据id删除期刊记录")
    @DeleteMapping("/{id}")
    public AjaxResponse deleteJournal(@NotNull(message = "id不能为空") @PathVariable int id) {
        // TODO: 检查用户权限
        log.info("deleteJournal: {}", id);
        journalService.deleteJournal(id);
        return AjaxResponse.success();
    }
}
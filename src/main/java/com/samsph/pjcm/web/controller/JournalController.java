package com.samsph.pjcm.web.controller;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.model.Journal;
import com.samsph.pjcm.service.JournalService;
import com.samsph.pjcm.query.Add;
import com.samsph.pjcm.query.JournalQuery;
import com.samsph.pjcm.query.Update;
import com.samsph.pjcm.vo.JournalVO;
import io.swagger.annotations.*;
import org.dozer.Mapper;
import org.springframework.data.domain.Page;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Resource;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

import static com.samsph.pjcm.config.DevUserId.ADMIN_ID;

/**
 * @author hujiahao
 */

@RestController
@Validated
@Api(tags = "1. 期刊管理")
@RequestMapping("/api/v1/journals")
public class JournalController {

    @Resource
    private Mapper dozerMapper;

    @Resource
    JournalService journalService;

    @Resource
    private CurrentUser currentUser;

    @PostMapping()
    @ApiOperation(value = "创建期刊")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @ApiImplicitParam(name = "journalQuery", value = "必填：year、month、day\n不填：id", dataType = "JournalQuery")
    public AjaxResponse saveJournal(@Validated({Add.class}) @RequestBody JournalQuery journalQuery) {
//        int uid = currentUser.getCurrentUser().getUserId();
        int uid = ADMIN_ID;
        Journal journal = journalService.saveJournal(journalQuery, uid);
        return AjaxResponse.success(dozerMapper.map(journal, JournalVO.class));
    }

    @PutMapping()
    @ApiOperation(value = "更新期刊")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @ApiImplicitParam(name = "journalQuery", value = "必填：id\n选填：year、month、day", dataType = "JournalQuery")
    public AjaxResponse updateJournal(@Validated({Update.class}) @RequestBody JournalQuery journalQuery) {
        // 更新期刊信息
        Journal journal = journalService.getJournal(journalQuery.getId());
        BeanUtil.copyProperties(journalQuery, journal, CopyOptions.create().setIgnoreNullValue(true).setIgnoreError(true));
        journalService.updateJournal(journal);
        return AjaxResponse.success();
    }


    @GetMapping()
    @ApiOperation(value = "获取期刊列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "number", value = "分页页号", required = true, dataType = "int"),
            @ApiImplicitParam(name = "size", value = "分页大小", required = true, dataType = "int"),
            @ApiImplicitParam(name = "ascend", value = "是否升序，默认为true", dataType = "boolean"),
    })
    public AjaxResponse getAll(@NotNull(message = "page不能为空") @RequestParam("number") Integer number,
                               @NotNull(message = "size不能为空") @RequestParam("size") Integer size,
                               @RequestParam(value = "ascend", required = false) Boolean ascend) {
        if (ascend == null) {
            ascend = true;
        }

        Page<Journal> journalPage = journalService.getAll(number, size, ascend);

        return AjaxResponse.success(DozerUtil.mapPage(journalPage, JournalVO.class));
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "根据id获取期刊详细信息")
    public AjaxResponse getJournal(@NotNull @PathVariable Integer id) {
        Journal journal = journalService.getJournal(id);
        return AjaxResponse.success(dozerMapper.map(journal, JournalVO.class));
    }

    @GetMapping("/")
    @ApiOperation(value = "根据年月卷期号获取期刊详细信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "year", required = true, value = "年号，2000到2050", dataType = "int"),
            @ApiImplicitParam(name = "month", required = true, value = "月号，1到12", dataType = "int"),
            @ApiImplicitParam(name = "volume", required = true, value = "卷号，至少为1", dataType = "int"),
            @ApiImplicitParam(name = "number", required = true, value = "期号，至少为1", dataType = "int")
    })
    public AjaxResponse getJournal2(@NotNull @Max(value = 2050, message = "年号至大为2050") @Min(value = 2000, message = "年号至少为2000") @RequestParam("year") Integer year,
                                    @NotNull @Max(value = 12, message = "月号至大为12") @Min(value = 1, message = "月号至少为1") @RequestParam("month") Integer month,
                                    @NotNull @Min(value = 1, message = "卷号至少为1") @RequestParam("volume") Integer volume,
                                    @NotNull @Min(value = 1, message = "期号至少为1") @RequestParam("number") Integer number) {
        Journal journal = journalService.getJournal(year, month, volume, number);
        return AjaxResponse.success(dozerMapper.map(journal, JournalVO.class));
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @ApiOperation(value = "管理员根据id删除期刊记录")
    public AjaxResponse deleteJournal(@NotNull(message = "id不能为空") @PathVariable Integer id) {
        journalService.deleteJournal(id);
        return AjaxResponse.success();
    }
}

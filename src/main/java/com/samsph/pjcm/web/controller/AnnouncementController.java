package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.model.Announcement;
import com.samsph.pjcm.service.AnnouncementService;
import com.samsph.pjcm.vo.AnnouncementVoGet;
import com.samsph.pjcm.vo.AnnouncementVoPost;
import com.samsph.pjcm.vo.AnnouncementVoPut;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import javax.validation.Valid;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.sql.Date;
import java.util.List;

@Api(tags = "公告管理")
@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class AnnouncementController {
    @Autowired
    private AnnouncementService announcementService;
    @Autowired
    private CurrentUser currentUser;

    @ApiOperation(value = "添加公告")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @PostMapping("/announcements")
    public AjaxResponse addAnnouncement(@Valid @RequestBody AnnouncementVoPost announcementVoPost){
        //权限检测TODO 用户身份是管理员且登录
        Announcement announcement = new Announcement();
        Date time = new Date(new java.util.Date().getTime());
        announcement.setCreateTime(time);
        announcement.setTitle(announcementVoPost.getTitle());
        announcement.setContent(announcementVoPost.getContent());
        //定义为登录后管理员的id
//        announcement.setCreateByUid(currentUser.getCurrentUser().getUserId());
        announcement.setCreateByUid(7);
        announcementService.addAnnouncement(announcement);
        return AjaxResponse.success();
    }

    @ApiOperation(value = "更新公告")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @PutMapping("/announcements")
    public AjaxResponse updateAnnouncement(@Valid @RequestBody AnnouncementVoPut announcementVoPut){
        //id为空或者id不存在
        if(!announcementService.findAnnouncement(announcementVoPut.getId()).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"公告id无效或未输入公告id!!!!");
        }
        else{
            Announcement announcement = announcementService.findAnnouncement(announcementVoPut.getId()).get();
            if((announcementVoPut.getTitle() != null)){
                announcement.setTitle(announcementVoPut.getTitle());
            }
            if((announcementVoPut.getContent() != null)){
                announcement.setContent(announcementVoPut.getContent());
            }
            announcementService.updateAnnouncement(announcement);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "删除公告")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="公告id")
    })
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    @DeleteMapping("/announcements/id={id}")
    public AjaxResponse deleteAnnouncement(@NotNull(message = "id不能为空")@Min(value = 1,message = "公告id必须是正整数") @PathVariable Integer id){
        //id为空或者id不存在
        if(!announcementService.findAnnouncement(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"公告id无效或未输入公告id!!!!");
        }else {
            announcementService.deleteAnnouncement(id);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "获取单个公告")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="公告id")
    })
    @GetMapping("/announcements/id={id}")
    public AjaxResponse findAnnouncement(@NotNull(message = "id不能为空")@Min(value = 1,message = "公告id必须是正整数") @PathVariable Integer id){
        if(announcementService.findAnnouncement(id).isPresent()){
            Announcement announcement = announcementService.findAnnouncement(id).get();
            return AjaxResponse.success(DozerUtil.map(announcement,AnnouncementVoGet.class));
        }else{
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"公告id无效或未输入公告id!!!!");
        }
    }

    @ApiOperation(value = "获取公告列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/announcements")
    public AjaxResponse findAnnouncements(@NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数")@RequestParam("page") Integer page,
                                          @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数")@RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size, Sort.Direction.DESC,"createTime");
        Page<Announcement> announcements = announcementService.findAnnouncements(pageRequest);
        List<Announcement> announcementsList = announcements.getContent();
        List<AnnouncementVoGet> announcementVoGets = DozerUtil.mapList(announcementsList,AnnouncementVoGet.class);
        PageData pageData = new PageData(announcements.getTotalPages(), (int) announcements.getTotalElements(),page,announcementVoGets.size(),announcementVoGets);
        return AjaxResponse.success(pageData);
    }


}

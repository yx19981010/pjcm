package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.auth.CurrentUser;
import com.samsph.pjcm.config.constant.Field;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.service.EditorFieldService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.config.utils.DozerUtil;
import com.samsph.pjcm.vo.EditorFieldVoGetEditor;
import com.samsph.pjcm.vo.EditorFieldVoGetField;
import com.samsph.pjcm.vo.EditorFieldVoPost;
import com.samsph.pjcm.vo.EditorFieldVoPut;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.util.List;

@Api(tags = "编辑-领域管理")
@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class EditorFieldController {
    @Autowired
    private EditorFieldService editorFieldService;
    @Autowired
    private UserRoleService userRoleService;
    @Autowired
    private CurrentUser currentUser;

    @ApiOperation(value = "添加编辑-领域")
    @PostMapping("/editorFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse addEditorField(@Valid @RequestBody EditorFieldVoPost editorFieldVoPost){
        //权限检测 用户身份是管理员且登录
        //检测编辑id是否正确（是否有这个uid以及该uid是否是编辑）
        if(!userRoleService.findUserHasRole(editorFieldVoPost.getEditorUid(), RoleType.EDITOR_ROLE)){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"编辑id错误或编辑未激活!!!");
        }else{
            List<EditorField> list = editorFieldService.findByEditorUid(editorFieldVoPost.getEditorUid());
            if(list!=null && list.size()>0){
                for(EditorField editorField : list){
                    if(editorField.getField() == editorFieldVoPost.getField().getCode()){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"编辑已存在该领域");
                    }
                }
            }
            EditorField editorField = new EditorField();
            editorField.setEditorUid(editorFieldVoPost.getEditorUid());
            editorField.setField(editorFieldVoPost.getField().getCode());
            editorFieldService.addEditorField(editorField);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "修改编辑-领域")
    @PutMapping("/editorFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse updateEditorField(@Valid @RequestBody EditorFieldVoPut editorFieldVoPut){
        //权限检测 用户身份是管理员且登录
        if(!editorFieldService.findEditorField(editorFieldVoPut.getId()).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"id无效或未输入id!!!!");
        }else{
            EditorField editorField = editorFieldService.findEditorField(editorFieldVoPut.getId()).get();
            if(editorFieldVoPut.getEditorUid()!=null){
                if(!userRoleService.findUserHasRole(editorFieldVoPut.getEditorUid(),RoleType.EDITOR_ROLE)){
                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"编辑id错误或编辑未激活!!!");
                }else{
                    editorField.setEditorUid(editorFieldVoPut.getEditorUid());
                }
            }
            if(editorFieldVoPut.getField()!=null){
                editorField.setField(editorFieldVoPut.getField());
            }
            editorFieldService.updateEditorField(editorField);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "删除编辑-领域")
    @ApiImplicitParams({
            @ApiImplicitParam(name="id",value="编辑-领域id")
    })
    @DeleteMapping("/editorFields/id={id}")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse deleteEditorField(@NotNull(message = "id不能为空")@Min(value = 1,message = "id必须是正整数") @PathVariable Integer id){
        //权限检测 用户身份是管理员且登录
        if(!editorFieldService.findEditorField(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"id无效或未输入id!!!!");
        }else{
            editorFieldService.deleteEditorField(id);
            return AjaxResponse.success();
        }
    }

    @ApiOperation(value = "得到编辑-领域列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name="editorOrFieldId",value="编辑id或领域id"),
            @ApiImplicitParam(name="flag",value="标识，1代表查看该编辑所管的领域，2代表查看该领域下的所有编辑"),
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/editorFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN','ROLE_EDITOR')")
    public AjaxResponse findEditorFields(@NotNull(message = "id不能为空")@Min(value = 1,message = "编辑id或领域必须是正整数") @RequestParam("editorOrFieldId") Integer editorOrFieldId,
                                                   @NotNull(message = "flag不能为空")@Min(value = 1,message = "flag最小值为1")@Max(value = 2,message = "flag最大值为2") @RequestParam("flag") Integer flag,
                                                   @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                                   @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        if(flag == 1){
//            if(currentUser.getCurrentUser().getUserRole() == RoleType.EDITOR_ROLE){
//                if(currentUser.getCurrentUser().getUserId() != editorOrFieldId){
//                    throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权查看其他编辑的领域!!!");
//                }
//            }
            if(!userRoleService.findUserHasRole(editorOrFieldId,RoleType.EDITOR_ROLE)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"编辑id错误或编辑未激活!!!");
            }else {
                Page<EditorField> editorFieldPage = editorFieldService.findEditorFieldsByEditorUid(editorOrFieldId, pageRequest);
                List<EditorField> editorFields = editorFieldPage.getContent();
                List<EditorFieldVoGetField> editorFieldVoGetFields = DozerUtil.mapList(editorFields, EditorFieldVoGetField.class);
                PageData pageData = new PageData(editorFieldPage.getTotalPages(), (int) editorFieldPage.getTotalElements(), page, editorFieldVoGetFields.size(), editorFieldVoGetFields);
                return AjaxResponse.success(pageData);
            }
        }else{
//            if(currentUser.getCurrentUser().getUserRole() != RoleType.ADMIN_ROLE){
//                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"无权查看该领域下的编辑!!!");
//            }
            if(editorOrFieldId > Field.TOTAL_FIELD){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"领域id错误!!!");
            }else {
                Page<EditorField> editorFieldPage = editorFieldService.findEditorFieldsByFieldId(editorOrFieldId, pageRequest);
                List<EditorField> editorFields = editorFieldPage.getContent();
                List<EditorFieldVoGetEditor> editorFieldVoGetEditors = DozerUtil.mapList(editorFields, EditorFieldVoGetEditor.class);
                PageData pageData = new PageData(editorFieldPage.getTotalPages(), (int) editorFieldPage.getTotalElements(), page, editorFieldVoGetEditors.size(), editorFieldVoGetEditors);
                return AjaxResponse.success(pageData);
            }
        }
    }
}

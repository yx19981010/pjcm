package com.samsph.pjcm.controller;

import com.samsph.pjcm.config.PageData;
import com.samsph.pjcm.config.constant.Field;
import com.samsph.pjcm.config.constant.RoleType;
import com.samsph.pjcm.config.exception.AjaxResponse;
import com.samsph.pjcm.config.exception.CustomException;
import com.samsph.pjcm.config.exception.CustomExceptionType;
import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.User;
import com.samsph.pjcm.service.EditorFieldService;
import com.samsph.pjcm.service.UserRoleService;
import com.samsph.pjcm.service.UserService;
import com.samsph.pjcm.vo.*;
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
import java.util.ArrayList;
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
    private UserService userService;

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
                    if(editorField.getField().equals(editorFieldVoPost.getField())){
                        throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"编辑已存在该领域");
                    }
                }
            }
            EditorField editorField = new EditorField();
            editorField.setEditorUid(editorFieldVoPost.getEditorUid());
            editorField.setField(editorFieldVoPost.getField());
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

    @ApiOperation(value = "得到所有编辑-领域信息")
    @GetMapping("/editorFields/all")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse findAllEditorFields(){
        return AjaxResponse.success(editorFieldService.findAll());
    }

    @ApiOperation(value = "通过领域得到编辑-领域信息")
    @ApiImplicitParams({
            @ApiImplicitParam(name="fieldId",value="领域id"),
            @ApiImplicitParam(name="page",value="页数"),
            @ApiImplicitParam(name="size",value="大小")
    })
    @GetMapping("/editorFields")
    @PreAuthorize("hasAnyRole('ROLE_ADMIN')")
    public AjaxResponse findEditorFields(@NotNull(message = "id不能为空") @Min(value = Field.LEAST_FIELD,message = "领域id最小为"+Field.LEAST_FIELD) @Max(value = Field.TOTAL_FIELD,message = "领域id最大为"+Field.TOTAL_FIELD) @RequestParam("fieldId") Integer fieldId,
                                         @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                         @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        if(editorFieldService.findEditorFieldsByFieldId(fieldId, pageRequest) != null) {
            Page<EditorField> editorFieldPage = editorFieldService.findEditorFieldsByFieldId(fieldId, pageRequest);
            List<EditorField> editorFields = editorFieldPage.getContent();
            List<EditorFieldVoGetEditor> editorFieldVoGetEditors = new ArrayList<>();
            for (EditorField editorField : editorFields) {
                User user = userService.findUserByUid(editorField.getEditorUid()).get();
                EditorFieldVoGetEditor editorFieldVoGetEditor = new EditorFieldVoGetEditor();
                editorFieldVoGetEditor.setId(editorField.getId());
                editorFieldVoGetEditor.setEditorUid(editorField.getEditorUid());
                editorFieldVoGetEditor.setEmail(user.getEmail());
                editorFieldVoGetEditor.setUserName(user.getUserName());
                editorFieldVoGetEditors.add(editorFieldVoGetEditor);
            }
            PageData pageData = new PageData(editorFieldPage.getTotalPages(), (int) editorFieldPage.getTotalElements(), page, editorFieldVoGetEditors.size(), editorFieldVoGetEditors);
            return AjaxResponse.success(pageData);
        }else{
            return AjaxResponse.success(new PageData(0, 0, page, 0, new ArrayList<>()));
        }
    }
}

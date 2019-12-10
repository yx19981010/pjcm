package com.samsph.pjcm.web.controller;

import com.samsph.pjcm.config.constant.FieldTotal;
import com.samsph.pjcm.config.PageData;
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

@Validated
@RestController
@RequestMapping(value = "/api/v1")
public class EditorFieldController {
    @Autowired
    private EditorFieldService editorFieldService;
    @Autowired
    private UserRoleService userRoleService;

    @PostMapping("/editorfields")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse addEditorField(@Valid @RequestBody EditorFieldVoPost editorFieldVoPost){
        //权限检测 用户身份是管理员且登录
        //检测编辑id是否正确（是否有这个uid以及该uid是否是编辑）
        if(!userRoleService.findUserHasRole(editorFieldVoPost.getEditorUid(), RoleType.EDITOR_ROLE)){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"编辑id错误或编辑未激活!!!");
        }else{
            EditorField editorField = new EditorField();
            editorField.setEditorUid(editorFieldVoPost.getEditorUid());
            editorField.setField(editorFieldVoPost.getField());
            editorFieldService.addEditorField(editorField);
            return AjaxResponse.success();
        }
    }

    @PutMapping("/editorfields")
    @PreAuthorize("hasAnyRole('ROLE_1')")
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

    @DeleteMapping("/editorfields/id={id}")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse deleteEditorField(@NotNull(message = "id不能为空")@Min(value = 1,message = "id必须是正整数") @PathVariable Integer id){
        //权限检测 用户身份是管理员且登录
        if(!editorFieldService.findEditorField(id).isPresent()){
            throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"id无效或未输入id!!!!");
        }else{
            editorFieldService.deleteEditorField(id);
            return AjaxResponse.success();
        }
    }

    @GetMapping("/editorfields")
    @PreAuthorize("hasAnyRole('ROLE_1')")
    public AjaxResponse findEditorFields(@NotNull(message = "id不能为空")@Min(value = 1,message = "编辑id必须是正整数") @RequestParam("editorOrfeildId") Integer editorOrfeildId,
                                                   @NotNull(message = "flag不能为空")@Min(value = 1,message = "flag最小值为1")@Max(value = 2,message = "flag最大值为2") @RequestParam("flag") Integer flag,
                                                   @NotNull(message = "未传入页数")@Min(value = 1,message = "页数必须是正整数") @RequestParam("page") Integer page,
                                                   @NotNull(message = "未传入每页的大小")@Min(value = 1,message = "每页的大小必须是正整数") @RequestParam("size") Integer size){
        PageRequest pageRequest = PageRequest.of(page-1,size);
        if(flag == 1){
            if(!userRoleService.findUserHasRole(editorOrfeildId,RoleType.EDITOR_ROLE)){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"编辑id错误或编辑未激活!!!");
            }else {
                Page<EditorField> editorFieldPage = editorFieldService.findEditorFieldsByEditorUid(editorOrfeildId, pageRequest);
                List<EditorField> editorFields = editorFieldPage.getContent();
                List<EditorFieldVoGetField> editorFieldVoGetFields = DozerUtil.mapList(editorFields, EditorFieldVoGetField.class);
                PageData pageData = new PageData(editorFieldPage.getTotalPages(), (int) editorFieldPage.getTotalElements(), page, editorFieldVoGetFields.size(), editorFieldVoGetFields);
                return AjaxResponse.success(pageData);
            }
        }else{
            if(editorOrfeildId > FieldTotal.TOTAL_FIELD){
                throw new CustomException(CustomExceptionType.USER_INPUT_ERROR,"领域id错误!!!");
            }else {
                Page<EditorField> editorFieldPage = editorFieldService.findEditorFieldsByFieldId(editorOrfeildId, pageRequest);
                List<EditorField> editorFields = editorFieldPage.getContent();
                List<EditorFieldVoGetEditor> editorFieldVoGetEditors = DozerUtil.mapList(editorFields, EditorFieldVoGetEditor.class);
                PageData pageData = new PageData(editorFieldPage.getTotalPages(), (int) editorFieldPage.getTotalElements(), page, editorFieldVoGetEditors.size(), editorFieldVoGetEditors);
                return AjaxResponse.success(pageData);
            }
        }
    }
}

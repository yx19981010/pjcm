package com.samsph.pjcm.service;


import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.vo.EditorFieldVoGet;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;


public interface EditorFieldService {

     /**
      * 添加编辑领域
      * @param editorField 编辑领域实体
      */
     void addEditorField(EditorField editorField);

     /**
      * 更新编辑领域
      * @param editorField 编辑领域实体
      */
     void updateEditorField(EditorField editorField);

     /**
      * 删除编辑领域
      * @param id 编辑领域id
      */
     void deleteEditorField(int id);

     /**
      * 根据编辑领域id查找编辑领域
      * @param id 编辑领域id
      * @return 编辑-领域
      */
     Optional<EditorField> findEditorField(int id);

     /**
      * 根据编辑id查找编辑领域列表
      * @param editorUid 编辑id
      * @return 编辑领域列表
      */
     List<EditorField> findByEditorUid(Integer editorUid);

     /**
      * 根据领域id查找编辑领域列表
      * @param fieldId 领域id
      * @return 编辑领域列表
      */
     List<EditorField> findByFieldId(Integer fieldId);

     /**
      * 得到所有编辑的领域信息
      * @return 编辑领域列表
      */
     List<EditorFieldVoGet> findAll();

     /**
      * 根据领域id得到分页后的编辑领域列表
      * @param fieldId 领域id
      * @param pageable 分页参数
      * @return 分页编辑领域列表
      */
     Page<EditorField> findEditorFieldsByFieldId(int fieldId, Pageable pageable);
}

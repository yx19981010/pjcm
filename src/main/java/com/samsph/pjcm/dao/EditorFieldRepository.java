package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.Optional;

public interface EditorFieldRepository extends JpaRepository<EditorField,Integer>, JpaSpecificationExecutor<EditorField> {
    /**
     * 根据编辑id得到编辑-领域列表
     *
     * @param editorUid 编辑id
     * @return List<EditorField>
     */
    List<EditorField> findByEditorUid(Integer editorUid);

    /**
     * 根据领域id得到编辑-领域列表
     *
     * @param fieldId 领域id
     * @return List<EditorField>
     */
    List<EditorField> findByField(Integer fieldId);
}

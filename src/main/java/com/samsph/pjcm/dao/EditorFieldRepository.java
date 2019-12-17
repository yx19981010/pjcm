package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.User;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.Optional;

public interface EditorFieldRepository extends JpaRepository<EditorField,Integer>, JpaSpecificationExecutor<EditorField> {
    List<EditorField> findByEditorUid(Integer editorUid);

    List<EditorField> findByField(Integer fieldId);
}

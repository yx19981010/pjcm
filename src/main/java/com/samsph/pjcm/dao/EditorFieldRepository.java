package com.samsph.pjcm.dao;

import com.samsph.pjcm.model.EditorField;
import com.samsph.pjcm.model.User;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface EditorFieldRepository extends JpaRepository<EditorField,Integer> {
    List<EditorField> findByEditorUid(Integer editorUid);

    List<EditorField> findByField(Integer fieldId);
}

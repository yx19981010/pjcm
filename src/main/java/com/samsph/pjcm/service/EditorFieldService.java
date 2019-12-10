package com.samsph.pjcm.service;


import com.samsph.pjcm.model.EditorField;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;

import java.awt.print.Pageable;
import java.util.List;
import java.util.Optional;


public interface EditorFieldService {

    public void addEditorField(EditorField editorField);

    public void updateEditorField(EditorField editorField);

    public void deleteEditorField(int id);

    public Optional<EditorField> findEditorField(int id);

    public Page<EditorField> findEditorFieldsByEditorUid(int editorUid, PageRequest pageRequest);

    public Page<EditorField> findEditorFieldsByFieldId(int fieldId, PageRequest pageRequest);
}

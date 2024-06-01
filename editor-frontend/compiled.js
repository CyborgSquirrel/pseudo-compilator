/*eslint-disable block-scoped-var, id-length, no-control-regex, no-magic-numbers, no-prototype-builtins, no-redeclare, no-shadow, no-var, sort-vars*/
import * as $protobuf from "protobufjs/minimal";

// Common aliases
const $Reader = $protobuf.Reader, $Writer = $protobuf.Writer, $util = $protobuf.util;

// Exported root namespace
const $root = $protobuf.roots["default"] || ($protobuf.roots["default"] = {});

export const PostJob = $root.PostJob = (() => {

    /**
     * Properties of a PostJob.
     * @exports IPostJob
     * @interface IPostJob
     * @property {string|null} [code] PostJob code
     * @property {boolean|null} [languageEnableLists] PostJob languageEnableLists
     */

    /**
     * Constructs a new PostJob.
     * @exports PostJob
     * @classdesc Represents a PostJob.
     * @implements IPostJob
     * @constructor
     * @param {IPostJob=} [properties] Properties to set
     */
    function PostJob(properties) {
        if (properties)
            for (let keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                if (properties[keys[i]] != null)
                    this[keys[i]] = properties[keys[i]];
    }

    /**
     * PostJob code.
     * @member {string} code
     * @memberof PostJob
     * @instance
     */
    PostJob.prototype.code = "";

    /**
     * PostJob languageEnableLists.
     * @member {boolean} languageEnableLists
     * @memberof PostJob
     * @instance
     */
    PostJob.prototype.languageEnableLists = false;

    /**
     * Creates a new PostJob instance using the specified properties.
     * @function create
     * @memberof PostJob
     * @static
     * @param {IPostJob=} [properties] Properties to set
     * @returns {PostJob} PostJob instance
     */
    PostJob.create = function create(properties) {
        return new PostJob(properties);
    };

    /**
     * Encodes the specified PostJob message. Does not implicitly {@link PostJob.verify|verify} messages.
     * @function encode
     * @memberof PostJob
     * @static
     * @param {IPostJob} message PostJob message or plain object to encode
     * @param {$protobuf.Writer} [writer] Writer to encode to
     * @returns {$protobuf.Writer} Writer
     */
    PostJob.encode = function encode(message, writer) {
        if (!writer)
            writer = $Writer.create();
        if (message.code != null && Object.hasOwnProperty.call(message, "code"))
            writer.uint32(/* id 1, wireType 2 =*/10).string(message.code);
        if (message.languageEnableLists != null && Object.hasOwnProperty.call(message, "languageEnableLists"))
            writer.uint32(/* id 2, wireType 0 =*/16).bool(message.languageEnableLists);
        return writer;
    };

    /**
     * Encodes the specified PostJob message, length delimited. Does not implicitly {@link PostJob.verify|verify} messages.
     * @function encodeDelimited
     * @memberof PostJob
     * @static
     * @param {IPostJob} message PostJob message or plain object to encode
     * @param {$protobuf.Writer} [writer] Writer to encode to
     * @returns {$protobuf.Writer} Writer
     */
    PostJob.encodeDelimited = function encodeDelimited(message, writer) {
        return this.encode(message, writer).ldelim();
    };

    /**
     * Decodes a PostJob message from the specified reader or buffer.
     * @function decode
     * @memberof PostJob
     * @static
     * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
     * @param {number} [length] Message length if known beforehand
     * @returns {PostJob} PostJob
     * @throws {Error} If the payload is not a reader or valid buffer
     * @throws {$protobuf.util.ProtocolError} If required fields are missing
     */
    PostJob.decode = function decode(reader, length) {
        if (!(reader instanceof $Reader))
            reader = $Reader.create(reader);
        let end = length === undefined ? reader.len : reader.pos + length, message = new $root.PostJob();
        while (reader.pos < end) {
            let tag = reader.uint32();
            switch (tag >>> 3) {
            case 1: {
                    message.code = reader.string();
                    break;
                }
            case 2: {
                    message.languageEnableLists = reader.bool();
                    break;
                }
            default:
                reader.skipType(tag & 7);
                break;
            }
        }
        return message;
    };

    /**
     * Decodes a PostJob message from the specified reader or buffer, length delimited.
     * @function decodeDelimited
     * @memberof PostJob
     * @static
     * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
     * @returns {PostJob} PostJob
     * @throws {Error} If the payload is not a reader or valid buffer
     * @throws {$protobuf.util.ProtocolError} If required fields are missing
     */
    PostJob.decodeDelimited = function decodeDelimited(reader) {
        if (!(reader instanceof $Reader))
            reader = new $Reader(reader);
        return this.decode(reader, reader.uint32());
    };

    /**
     * Verifies a PostJob message.
     * @function verify
     * @memberof PostJob
     * @static
     * @param {Object.<string,*>} message Plain object to verify
     * @returns {string|null} `null` if valid, otherwise the reason why it is not
     */
    PostJob.verify = function verify(message) {
        if (typeof message !== "object" || message === null)
            return "object expected";
        if (message.code != null && message.hasOwnProperty("code"))
            if (!$util.isString(message.code))
                return "code: string expected";
        if (message.languageEnableLists != null && message.hasOwnProperty("languageEnableLists"))
            if (typeof message.languageEnableLists !== "boolean")
                return "languageEnableLists: boolean expected";
        return null;
    };

    /**
     * Creates a PostJob message from a plain object. Also converts values to their respective internal types.
     * @function fromObject
     * @memberof PostJob
     * @static
     * @param {Object.<string,*>} object Plain object
     * @returns {PostJob} PostJob
     */
    PostJob.fromObject = function fromObject(object) {
        if (object instanceof $root.PostJob)
            return object;
        let message = new $root.PostJob();
        if (object.code != null)
            message.code = String(object.code);
        if (object.languageEnableLists != null)
            message.languageEnableLists = Boolean(object.languageEnableLists);
        return message;
    };

    /**
     * Creates a plain object from a PostJob message. Also converts values to other types if specified.
     * @function toObject
     * @memberof PostJob
     * @static
     * @param {PostJob} message PostJob
     * @param {$protobuf.IConversionOptions} [options] Conversion options
     * @returns {Object.<string,*>} Plain object
     */
    PostJob.toObject = function toObject(message, options) {
        if (!options)
            options = {};
        let object = {};
        if (options.defaults) {
            object.code = "";
            object.languageEnableLists = false;
        }
        if (message.code != null && message.hasOwnProperty("code"))
            object.code = message.code;
        if (message.languageEnableLists != null && message.hasOwnProperty("languageEnableLists"))
            object.languageEnableLists = message.languageEnableLists;
        return object;
    };

    /**
     * Converts this PostJob to JSON.
     * @function toJSON
     * @memberof PostJob
     * @instance
     * @returns {Object.<string,*>} JSON object
     */
    PostJob.prototype.toJSON = function toJSON() {
        return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
    };

    /**
     * Gets the default type url for PostJob
     * @function getTypeUrl
     * @memberof PostJob
     * @static
     * @param {string} [typeUrlPrefix] your custom typeUrlPrefix(default "type.googleapis.com")
     * @returns {string} The default type url
     */
    PostJob.getTypeUrl = function getTypeUrl(typeUrlPrefix) {
        if (typeUrlPrefix === undefined) {
            typeUrlPrefix = "type.googleapis.com";
        }
        return typeUrlPrefix + "/PostJob";
    };

    return PostJob;
})();

export const JobResponse = $root.JobResponse = (() => {

    /**
     * Properties of a JobResponse.
     * @exports IJobResponse
     * @interface IJobResponse
     * @property {JobResponse.ISuccess|null} [success] JobResponse success
     * @property {JobResponse.IFailure|null} [failure] JobResponse failure
     */

    /**
     * Constructs a new JobResponse.
     * @exports JobResponse
     * @classdesc Represents a JobResponse.
     * @implements IJobResponse
     * @constructor
     * @param {IJobResponse=} [properties] Properties to set
     */
    function JobResponse(properties) {
        if (properties)
            for (let keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                if (properties[keys[i]] != null)
                    this[keys[i]] = properties[keys[i]];
    }

    /**
     * JobResponse success.
     * @member {JobResponse.ISuccess|null|undefined} success
     * @memberof JobResponse
     * @instance
     */
    JobResponse.prototype.success = null;

    /**
     * JobResponse failure.
     * @member {JobResponse.IFailure|null|undefined} failure
     * @memberof JobResponse
     * @instance
     */
    JobResponse.prototype.failure = null;

    // OneOf field names bound to virtual getters and setters
    let $oneOfFields;

    /**
     * JobResponse result.
     * @member {"success"|"failure"|undefined} result
     * @memberof JobResponse
     * @instance
     */
    Object.defineProperty(JobResponse.prototype, "result", {
        get: $util.oneOfGetter($oneOfFields = ["success", "failure"]),
        set: $util.oneOfSetter($oneOfFields)
    });

    /**
     * Creates a new JobResponse instance using the specified properties.
     * @function create
     * @memberof JobResponse
     * @static
     * @param {IJobResponse=} [properties] Properties to set
     * @returns {JobResponse} JobResponse instance
     */
    JobResponse.create = function create(properties) {
        return new JobResponse(properties);
    };

    /**
     * Encodes the specified JobResponse message. Does not implicitly {@link JobResponse.verify|verify} messages.
     * @function encode
     * @memberof JobResponse
     * @static
     * @param {IJobResponse} message JobResponse message or plain object to encode
     * @param {$protobuf.Writer} [writer] Writer to encode to
     * @returns {$protobuf.Writer} Writer
     */
    JobResponse.encode = function encode(message, writer) {
        if (!writer)
            writer = $Writer.create();
        if (message.success != null && Object.hasOwnProperty.call(message, "success"))
            $root.JobResponse.Success.encode(message.success, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
        if (message.failure != null && Object.hasOwnProperty.call(message, "failure"))
            $root.JobResponse.Failure.encode(message.failure, writer.uint32(/* id 2, wireType 2 =*/18).fork()).ldelim();
        return writer;
    };

    /**
     * Encodes the specified JobResponse message, length delimited. Does not implicitly {@link JobResponse.verify|verify} messages.
     * @function encodeDelimited
     * @memberof JobResponse
     * @static
     * @param {IJobResponse} message JobResponse message or plain object to encode
     * @param {$protobuf.Writer} [writer] Writer to encode to
     * @returns {$protobuf.Writer} Writer
     */
    JobResponse.encodeDelimited = function encodeDelimited(message, writer) {
        return this.encode(message, writer).ldelim();
    };

    /**
     * Decodes a JobResponse message from the specified reader or buffer.
     * @function decode
     * @memberof JobResponse
     * @static
     * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
     * @param {number} [length] Message length if known beforehand
     * @returns {JobResponse} JobResponse
     * @throws {Error} If the payload is not a reader or valid buffer
     * @throws {$protobuf.util.ProtocolError} If required fields are missing
     */
    JobResponse.decode = function decode(reader, length) {
        if (!(reader instanceof $Reader))
            reader = $Reader.create(reader);
        let end = length === undefined ? reader.len : reader.pos + length, message = new $root.JobResponse();
        while (reader.pos < end) {
            let tag = reader.uint32();
            switch (tag >>> 3) {
            case 1: {
                    message.success = $root.JobResponse.Success.decode(reader, reader.uint32());
                    break;
                }
            case 2: {
                    message.failure = $root.JobResponse.Failure.decode(reader, reader.uint32());
                    break;
                }
            default:
                reader.skipType(tag & 7);
                break;
            }
        }
        return message;
    };

    /**
     * Decodes a JobResponse message from the specified reader or buffer, length delimited.
     * @function decodeDelimited
     * @memberof JobResponse
     * @static
     * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
     * @returns {JobResponse} JobResponse
     * @throws {Error} If the payload is not a reader or valid buffer
     * @throws {$protobuf.util.ProtocolError} If required fields are missing
     */
    JobResponse.decodeDelimited = function decodeDelimited(reader) {
        if (!(reader instanceof $Reader))
            reader = new $Reader(reader);
        return this.decode(reader, reader.uint32());
    };

    /**
     * Verifies a JobResponse message.
     * @function verify
     * @memberof JobResponse
     * @static
     * @param {Object.<string,*>} message Plain object to verify
     * @returns {string|null} `null` if valid, otherwise the reason why it is not
     */
    JobResponse.verify = function verify(message) {
        if (typeof message !== "object" || message === null)
            return "object expected";
        let properties = {};
        if (message.success != null && message.hasOwnProperty("success")) {
            properties.result = 1;
            {
                let error = $root.JobResponse.Success.verify(message.success);
                if (error)
                    return "success." + error;
            }
        }
        if (message.failure != null && message.hasOwnProperty("failure")) {
            if (properties.result === 1)
                return "result: multiple values";
            properties.result = 1;
            {
                let error = $root.JobResponse.Failure.verify(message.failure);
                if (error)
                    return "failure." + error;
            }
        }
        return null;
    };

    /**
     * Creates a JobResponse message from a plain object. Also converts values to their respective internal types.
     * @function fromObject
     * @memberof JobResponse
     * @static
     * @param {Object.<string,*>} object Plain object
     * @returns {JobResponse} JobResponse
     */
    JobResponse.fromObject = function fromObject(object) {
        if (object instanceof $root.JobResponse)
            return object;
        let message = new $root.JobResponse();
        if (object.success != null) {
            if (typeof object.success !== "object")
                throw TypeError(".JobResponse.success: object expected");
            message.success = $root.JobResponse.Success.fromObject(object.success);
        }
        if (object.failure != null) {
            if (typeof object.failure !== "object")
                throw TypeError(".JobResponse.failure: object expected");
            message.failure = $root.JobResponse.Failure.fromObject(object.failure);
        }
        return message;
    };

    /**
     * Creates a plain object from a JobResponse message. Also converts values to other types if specified.
     * @function toObject
     * @memberof JobResponse
     * @static
     * @param {JobResponse} message JobResponse
     * @param {$protobuf.IConversionOptions} [options] Conversion options
     * @returns {Object.<string,*>} Plain object
     */
    JobResponse.toObject = function toObject(message, options) {
        if (!options)
            options = {};
        let object = {};
        if (message.success != null && message.hasOwnProperty("success")) {
            object.success = $root.JobResponse.Success.toObject(message.success, options);
            if (options.oneofs)
                object.result = "success";
        }
        if (message.failure != null && message.hasOwnProperty("failure")) {
            object.failure = $root.JobResponse.Failure.toObject(message.failure, options);
            if (options.oneofs)
                object.result = "failure";
        }
        return object;
    };

    /**
     * Converts this JobResponse to JSON.
     * @function toJSON
     * @memberof JobResponse
     * @instance
     * @returns {Object.<string,*>} JSON object
     */
    JobResponse.prototype.toJSON = function toJSON() {
        return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
    };

    /**
     * Gets the default type url for JobResponse
     * @function getTypeUrl
     * @memberof JobResponse
     * @static
     * @param {string} [typeUrlPrefix] your custom typeUrlPrefix(default "type.googleapis.com")
     * @returns {string} The default type url
     */
    JobResponse.getTypeUrl = function getTypeUrl(typeUrlPrefix) {
        if (typeUrlPrefix === undefined) {
            typeUrlPrefix = "type.googleapis.com";
        }
        return typeUrlPrefix + "/JobResponse";
    };

    JobResponse.Success = (function() {

        /**
         * Properties of a Success.
         * @memberof JobResponse
         * @interface ISuccess
         * @property {number|null} [jobId] Success jobId
         */

        /**
         * Constructs a new Success.
         * @memberof JobResponse
         * @classdesc Represents a Success.
         * @implements ISuccess
         * @constructor
         * @param {JobResponse.ISuccess=} [properties] Properties to set
         */
        function Success(properties) {
            if (properties)
                for (let keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }

        /**
         * Success jobId.
         * @member {number} jobId
         * @memberof JobResponse.Success
         * @instance
         */
        Success.prototype.jobId = 0;

        /**
         * Creates a new Success instance using the specified properties.
         * @function create
         * @memberof JobResponse.Success
         * @static
         * @param {JobResponse.ISuccess=} [properties] Properties to set
         * @returns {JobResponse.Success} Success instance
         */
        Success.create = function create(properties) {
            return new Success(properties);
        };

        /**
         * Encodes the specified Success message. Does not implicitly {@link JobResponse.Success.verify|verify} messages.
         * @function encode
         * @memberof JobResponse.Success
         * @static
         * @param {JobResponse.ISuccess} message Success message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Success.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.jobId != null && Object.hasOwnProperty.call(message, "jobId"))
                writer.uint32(/* id 1, wireType 0 =*/8).int32(message.jobId);
            return writer;
        };

        /**
         * Encodes the specified Success message, length delimited. Does not implicitly {@link JobResponse.Success.verify|verify} messages.
         * @function encodeDelimited
         * @memberof JobResponse.Success
         * @static
         * @param {JobResponse.ISuccess} message Success message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Success.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };

        /**
         * Decodes a Success message from the specified reader or buffer.
         * @function decode
         * @memberof JobResponse.Success
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {JobResponse.Success} Success
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Success.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            let end = length === undefined ? reader.len : reader.pos + length, message = new $root.JobResponse.Success();
            while (reader.pos < end) {
                let tag = reader.uint32();
                switch (tag >>> 3) {
                case 1: {
                        message.jobId = reader.int32();
                        break;
                    }
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };

        /**
         * Decodes a Success message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof JobResponse.Success
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {JobResponse.Success} Success
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Success.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };

        /**
         * Verifies a Success message.
         * @function verify
         * @memberof JobResponse.Success
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Success.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.jobId != null && message.hasOwnProperty("jobId"))
                if (!$util.isInteger(message.jobId))
                    return "jobId: integer expected";
            return null;
        };

        /**
         * Creates a Success message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof JobResponse.Success
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {JobResponse.Success} Success
         */
        Success.fromObject = function fromObject(object) {
            if (object instanceof $root.JobResponse.Success)
                return object;
            let message = new $root.JobResponse.Success();
            if (object.jobId != null)
                message.jobId = object.jobId | 0;
            return message;
        };

        /**
         * Creates a plain object from a Success message. Also converts values to other types if specified.
         * @function toObject
         * @memberof JobResponse.Success
         * @static
         * @param {JobResponse.Success} message Success
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Success.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            let object = {};
            if (options.defaults)
                object.jobId = 0;
            if (message.jobId != null && message.hasOwnProperty("jobId"))
                object.jobId = message.jobId;
            return object;
        };

        /**
         * Converts this Success to JSON.
         * @function toJSON
         * @memberof JobResponse.Success
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Success.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };

        /**
         * Gets the default type url for Success
         * @function getTypeUrl
         * @memberof JobResponse.Success
         * @static
         * @param {string} [typeUrlPrefix] your custom typeUrlPrefix(default "type.googleapis.com")
         * @returns {string} The default type url
         */
        Success.getTypeUrl = function getTypeUrl(typeUrlPrefix) {
            if (typeUrlPrefix === undefined) {
                typeUrlPrefix = "type.googleapis.com";
            }
            return typeUrlPrefix + "/JobResponse.Success";
        };

        return Success;
    })();

    JobResponse.Failure = (function() {

        /**
         * Properties of a Failure.
         * @memberof JobResponse
         * @interface IFailure
         * @property {JobResponse.Failure.IParserError|null} [error] Failure error
         */

        /**
         * Constructs a new Failure.
         * @memberof JobResponse
         * @classdesc Represents a Failure.
         * @implements IFailure
         * @constructor
         * @param {JobResponse.IFailure=} [properties] Properties to set
         */
        function Failure(properties) {
            if (properties)
                for (let keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                    if (properties[keys[i]] != null)
                        this[keys[i]] = properties[keys[i]];
        }

        /**
         * Failure error.
         * @member {JobResponse.Failure.IParserError|null|undefined} error
         * @memberof JobResponse.Failure
         * @instance
         */
        Failure.prototype.error = null;

        /**
         * Creates a new Failure instance using the specified properties.
         * @function create
         * @memberof JobResponse.Failure
         * @static
         * @param {JobResponse.IFailure=} [properties] Properties to set
         * @returns {JobResponse.Failure} Failure instance
         */
        Failure.create = function create(properties) {
            return new Failure(properties);
        };

        /**
         * Encodes the specified Failure message. Does not implicitly {@link JobResponse.Failure.verify|verify} messages.
         * @function encode
         * @memberof JobResponse.Failure
         * @static
         * @param {JobResponse.IFailure} message Failure message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Failure.encode = function encode(message, writer) {
            if (!writer)
                writer = $Writer.create();
            if (message.error != null && Object.hasOwnProperty.call(message, "error"))
                $root.JobResponse.Failure.ParserError.encode(message.error, writer.uint32(/* id 1, wireType 2 =*/10).fork()).ldelim();
            return writer;
        };

        /**
         * Encodes the specified Failure message, length delimited. Does not implicitly {@link JobResponse.Failure.verify|verify} messages.
         * @function encodeDelimited
         * @memberof JobResponse.Failure
         * @static
         * @param {JobResponse.IFailure} message Failure message or plain object to encode
         * @param {$protobuf.Writer} [writer] Writer to encode to
         * @returns {$protobuf.Writer} Writer
         */
        Failure.encodeDelimited = function encodeDelimited(message, writer) {
            return this.encode(message, writer).ldelim();
        };

        /**
         * Decodes a Failure message from the specified reader or buffer.
         * @function decode
         * @memberof JobResponse.Failure
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @param {number} [length] Message length if known beforehand
         * @returns {JobResponse.Failure} Failure
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Failure.decode = function decode(reader, length) {
            if (!(reader instanceof $Reader))
                reader = $Reader.create(reader);
            let end = length === undefined ? reader.len : reader.pos + length, message = new $root.JobResponse.Failure();
            while (reader.pos < end) {
                let tag = reader.uint32();
                switch (tag >>> 3) {
                case 1: {
                        message.error = $root.JobResponse.Failure.ParserError.decode(reader, reader.uint32());
                        break;
                    }
                default:
                    reader.skipType(tag & 7);
                    break;
                }
            }
            return message;
        };

        /**
         * Decodes a Failure message from the specified reader or buffer, length delimited.
         * @function decodeDelimited
         * @memberof JobResponse.Failure
         * @static
         * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
         * @returns {JobResponse.Failure} Failure
         * @throws {Error} If the payload is not a reader or valid buffer
         * @throws {$protobuf.util.ProtocolError} If required fields are missing
         */
        Failure.decodeDelimited = function decodeDelimited(reader) {
            if (!(reader instanceof $Reader))
                reader = new $Reader(reader);
            return this.decode(reader, reader.uint32());
        };

        /**
         * Verifies a Failure message.
         * @function verify
         * @memberof JobResponse.Failure
         * @static
         * @param {Object.<string,*>} message Plain object to verify
         * @returns {string|null} `null` if valid, otherwise the reason why it is not
         */
        Failure.verify = function verify(message) {
            if (typeof message !== "object" || message === null)
                return "object expected";
            if (message.error != null && message.hasOwnProperty("error")) {
                let error = $root.JobResponse.Failure.ParserError.verify(message.error);
                if (error)
                    return "error." + error;
            }
            return null;
        };

        /**
         * Creates a Failure message from a plain object. Also converts values to their respective internal types.
         * @function fromObject
         * @memberof JobResponse.Failure
         * @static
         * @param {Object.<string,*>} object Plain object
         * @returns {JobResponse.Failure} Failure
         */
        Failure.fromObject = function fromObject(object) {
            if (object instanceof $root.JobResponse.Failure)
                return object;
            let message = new $root.JobResponse.Failure();
            if (object.error != null) {
                if (typeof object.error !== "object")
                    throw TypeError(".JobResponse.Failure.error: object expected");
                message.error = $root.JobResponse.Failure.ParserError.fromObject(object.error);
            }
            return message;
        };

        /**
         * Creates a plain object from a Failure message. Also converts values to other types if specified.
         * @function toObject
         * @memberof JobResponse.Failure
         * @static
         * @param {JobResponse.Failure} message Failure
         * @param {$protobuf.IConversionOptions} [options] Conversion options
         * @returns {Object.<string,*>} Plain object
         */
        Failure.toObject = function toObject(message, options) {
            if (!options)
                options = {};
            let object = {};
            if (options.defaults)
                object.error = null;
            if (message.error != null && message.hasOwnProperty("error"))
                object.error = $root.JobResponse.Failure.ParserError.toObject(message.error, options);
            return object;
        };

        /**
         * Converts this Failure to JSON.
         * @function toJSON
         * @memberof JobResponse.Failure
         * @instance
         * @returns {Object.<string,*>} JSON object
         */
        Failure.prototype.toJSON = function toJSON() {
            return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
        };

        /**
         * Gets the default type url for Failure
         * @function getTypeUrl
         * @memberof JobResponse.Failure
         * @static
         * @param {string} [typeUrlPrefix] your custom typeUrlPrefix(default "type.googleapis.com")
         * @returns {string} The default type url
         */
        Failure.getTypeUrl = function getTypeUrl(typeUrlPrefix) {
            if (typeUrlPrefix === undefined) {
                typeUrlPrefix = "type.googleapis.com";
            }
            return typeUrlPrefix + "/JobResponse.Failure";
        };

        Failure.ParserError = (function() {

            /**
             * Properties of a ParserError.
             * @memberof JobResponse.Failure
             * @interface IParserError
             * @property {number|null} [line] ParserError line
             * @property {number|null} [column] ParserError column
             * @property {string|null} [message] ParserError message
             */

            /**
             * Constructs a new ParserError.
             * @memberof JobResponse.Failure
             * @classdesc Represents a ParserError.
             * @implements IParserError
             * @constructor
             * @param {JobResponse.Failure.IParserError=} [properties] Properties to set
             */
            function ParserError(properties) {
                if (properties)
                    for (let keys = Object.keys(properties), i = 0; i < keys.length; ++i)
                        if (properties[keys[i]] != null)
                            this[keys[i]] = properties[keys[i]];
            }

            /**
             * ParserError line.
             * @member {number} line
             * @memberof JobResponse.Failure.ParserError
             * @instance
             */
            ParserError.prototype.line = 0;

            /**
             * ParserError column.
             * @member {number} column
             * @memberof JobResponse.Failure.ParserError
             * @instance
             */
            ParserError.prototype.column = 0;

            /**
             * ParserError message.
             * @member {string} message
             * @memberof JobResponse.Failure.ParserError
             * @instance
             */
            ParserError.prototype.message = "";

            /**
             * Creates a new ParserError instance using the specified properties.
             * @function create
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {JobResponse.Failure.IParserError=} [properties] Properties to set
             * @returns {JobResponse.Failure.ParserError} ParserError instance
             */
            ParserError.create = function create(properties) {
                return new ParserError(properties);
            };

            /**
             * Encodes the specified ParserError message. Does not implicitly {@link JobResponse.Failure.ParserError.verify|verify} messages.
             * @function encode
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {JobResponse.Failure.IParserError} message ParserError message or plain object to encode
             * @param {$protobuf.Writer} [writer] Writer to encode to
             * @returns {$protobuf.Writer} Writer
             */
            ParserError.encode = function encode(message, writer) {
                if (!writer)
                    writer = $Writer.create();
                if (message.line != null && Object.hasOwnProperty.call(message, "line"))
                    writer.uint32(/* id 1, wireType 0 =*/8).int32(message.line);
                if (message.column != null && Object.hasOwnProperty.call(message, "column"))
                    writer.uint32(/* id 2, wireType 0 =*/16).int32(message.column);
                if (message.message != null && Object.hasOwnProperty.call(message, "message"))
                    writer.uint32(/* id 3, wireType 2 =*/26).string(message.message);
                return writer;
            };

            /**
             * Encodes the specified ParserError message, length delimited. Does not implicitly {@link JobResponse.Failure.ParserError.verify|verify} messages.
             * @function encodeDelimited
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {JobResponse.Failure.IParserError} message ParserError message or plain object to encode
             * @param {$protobuf.Writer} [writer] Writer to encode to
             * @returns {$protobuf.Writer} Writer
             */
            ParserError.encodeDelimited = function encodeDelimited(message, writer) {
                return this.encode(message, writer).ldelim();
            };

            /**
             * Decodes a ParserError message from the specified reader or buffer.
             * @function decode
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
             * @param {number} [length] Message length if known beforehand
             * @returns {JobResponse.Failure.ParserError} ParserError
             * @throws {Error} If the payload is not a reader or valid buffer
             * @throws {$protobuf.util.ProtocolError} If required fields are missing
             */
            ParserError.decode = function decode(reader, length) {
                if (!(reader instanceof $Reader))
                    reader = $Reader.create(reader);
                let end = length === undefined ? reader.len : reader.pos + length, message = new $root.JobResponse.Failure.ParserError();
                while (reader.pos < end) {
                    let tag = reader.uint32();
                    switch (tag >>> 3) {
                    case 1: {
                            message.line = reader.int32();
                            break;
                        }
                    case 2: {
                            message.column = reader.int32();
                            break;
                        }
                    case 3: {
                            message.message = reader.string();
                            break;
                        }
                    default:
                        reader.skipType(tag & 7);
                        break;
                    }
                }
                return message;
            };

            /**
             * Decodes a ParserError message from the specified reader or buffer, length delimited.
             * @function decodeDelimited
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {$protobuf.Reader|Uint8Array} reader Reader or buffer to decode from
             * @returns {JobResponse.Failure.ParserError} ParserError
             * @throws {Error} If the payload is not a reader or valid buffer
             * @throws {$protobuf.util.ProtocolError} If required fields are missing
             */
            ParserError.decodeDelimited = function decodeDelimited(reader) {
                if (!(reader instanceof $Reader))
                    reader = new $Reader(reader);
                return this.decode(reader, reader.uint32());
            };

            /**
             * Verifies a ParserError message.
             * @function verify
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {Object.<string,*>} message Plain object to verify
             * @returns {string|null} `null` if valid, otherwise the reason why it is not
             */
            ParserError.verify = function verify(message) {
                if (typeof message !== "object" || message === null)
                    return "object expected";
                if (message.line != null && message.hasOwnProperty("line"))
                    if (!$util.isInteger(message.line))
                        return "line: integer expected";
                if (message.column != null && message.hasOwnProperty("column"))
                    if (!$util.isInteger(message.column))
                        return "column: integer expected";
                if (message.message != null && message.hasOwnProperty("message"))
                    if (!$util.isString(message.message))
                        return "message: string expected";
                return null;
            };

            /**
             * Creates a ParserError message from a plain object. Also converts values to their respective internal types.
             * @function fromObject
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {Object.<string,*>} object Plain object
             * @returns {JobResponse.Failure.ParserError} ParserError
             */
            ParserError.fromObject = function fromObject(object) {
                if (object instanceof $root.JobResponse.Failure.ParserError)
                    return object;
                let message = new $root.JobResponse.Failure.ParserError();
                if (object.line != null)
                    message.line = object.line | 0;
                if (object.column != null)
                    message.column = object.column | 0;
                if (object.message != null)
                    message.message = String(object.message);
                return message;
            };

            /**
             * Creates a plain object from a ParserError message. Also converts values to other types if specified.
             * @function toObject
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {JobResponse.Failure.ParserError} message ParserError
             * @param {$protobuf.IConversionOptions} [options] Conversion options
             * @returns {Object.<string,*>} Plain object
             */
            ParserError.toObject = function toObject(message, options) {
                if (!options)
                    options = {};
                let object = {};
                if (options.defaults) {
                    object.line = 0;
                    object.column = 0;
                    object.message = "";
                }
                if (message.line != null && message.hasOwnProperty("line"))
                    object.line = message.line;
                if (message.column != null && message.hasOwnProperty("column"))
                    object.column = message.column;
                if (message.message != null && message.hasOwnProperty("message"))
                    object.message = message.message;
                return object;
            };

            /**
             * Converts this ParserError to JSON.
             * @function toJSON
             * @memberof JobResponse.Failure.ParserError
             * @instance
             * @returns {Object.<string,*>} JSON object
             */
            ParserError.prototype.toJSON = function toJSON() {
                return this.constructor.toObject(this, $protobuf.util.toJSONOptions);
            };

            /**
             * Gets the default type url for ParserError
             * @function getTypeUrl
             * @memberof JobResponse.Failure.ParserError
             * @static
             * @param {string} [typeUrlPrefix] your custom typeUrlPrefix(default "type.googleapis.com")
             * @returns {string} The default type url
             */
            ParserError.getTypeUrl = function getTypeUrl(typeUrlPrefix) {
                if (typeUrlPrefix === undefined) {
                    typeUrlPrefix = "type.googleapis.com";
                }
                return typeUrlPrefix + "/JobResponse.Failure.ParserError";
            };

            return ParserError;
        })();

        return Failure;
    })();

    return JobResponse;
})();

export { $root as default };
